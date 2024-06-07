transmit_results <-
  function(results_list,
           receiver,
           output_metric,
           trial_path,
           ed_df,
           warmup = 50,
           sim_length = 365,
           arg_list) {
    # Function that return the simulation results to return to the search algorithm
    origin_date <- as.POSIXct("2018-12-01 00:00:00", tz = "UTC") + (3600 * 24 * warmup)
    val_env <- readRDS(file.path("Data","Validation Metric Dataframes.rds"))
    if (grepl('admissions',output_metric)){
      resource_df <-
        results_list[[2]][is.na(`External Transfer`), `External Transfer` := FALSE][is.na(`ED Patient`), `ED Patient` := FALSE][order(replication, time, system)][, `:=`(date_time = as.datetime(time * 3600, origin_date),
                                                                                                                                                                         day_num = as.numeric(as.factor(lubridate::date(
                                                                                                                                                                           as.datetime(time * 3600, origin_date)
                                                                                                                                                                         )))),
                                                                                                                                                                  by = replication][day_num >= warmup][, `:=`(occupancy = 100 * (server /
                                                                                                                                                                                                                                   capacity),
                                                                                                                                                                                                              Delta = abs(as.numeric(
                                                                                                                                                                                                                difftime(date_time, data.table::shift(date_time, 1),
                                                                                                                                                                                                                         units = 'hours')
                                                                                                                                                                                                              ))),
                                                                                                                                                                                                       by = list(resource, replication)]
      facility_admissions_df <-
        copy(resource_df)[cap_change > 0 &
                            is.na(patient) &
                            day_num > warmup, patient := generate_random_id(13)][cap_change > 0 &
                                                                                   day_num > warmup, ][siteInfo, facility := Facility_name, on = c('resource' = 'Bed_Group')][, is_Adult := Age %in% c('Adult', 'Geriatric')][, .(admissions = length(unique(patient))), by = list(replication, facility, is_Adult)][is.na(admissions), admissions := 0][, `:=`(grouping = as.character(
                                                                                     ifelse(is_Adult, "adult_admissions", "pediatric_admissions")
                                                                                   ), is_Adult = NULL)]
      
      total_admission_df <-
        copy(resource_df)[cap_change > 0 &
                            is.na(patient) &
                            day_num > warmup, patient := generate_random_id(13)][cap_change > 0 &
                                                                                   day_num > warmup, ][siteInfo, facility := Facility_name, on = c('resource' = 'Bed_Group')][, is_Adult := Age %in% c('Adult', 'Geriatric')][, .(admissions = length(unique(patient))), by = list(replication, facility, is_Adult)][is.na(admissions), admissions := 0][, .(facility = 'All', admissions = sum(admissions)), by = list(is_Adult, replication)][, `:=`(grouping = as.character(
                                                                                     ifelse(is_Adult, "adult_admissions", "pediatric_admissions")
                                                                                   ), is_Adult = NULL)]
      
      facility_admissions_df <-
        rbind(
          facility_admissions_df,
          melt(
            facility_admissions_df[, .(total_admissions = sum(admissions)), by = list(replication, facility)],
            id.vars = c('replication', 'facility'),
            variable.name = 'grouping',
            value.name = 'admissions'
          )
        )
      total_admission_df <-
        rbind(
          total_admission_df,
          melt(
            total_admission_df[, .(total_admissions = sum(admissions)), by = list(replication, facility)],
            id.vars = c('replication', 'facility'),
            variable.name = 'grouping',
            value.name = 'admissions'
          )
        )
      hccis_melt <-
        melt(
          copy(ed_df)[, rpt_year := NULL],
          id.vars = c('hccis_id', 'owner_hccis_id', 'urban'),
          variable.name = "grouping",
          value.name = 'value'
        )[, grouping := tolower(grouping)]
      hccis_melt <-
        rbind(hccis_melt, hccis_melt[, .(value = sum(value)), by = grouping][grepl('admissions', grouping) &
                                                                               !grepl('icu|ed_to_ip', grouping)][, hccis_id := 'All'], fill = TRUE)
      admissions_by_facility <-
        rbind(facility_admissions_df, total_admission_df, fill = TRUE)[hccis_melt, hccis_admissions := (value/365) * sim_length, on = c(facility = 'hccis_id',grouping = 'grouping')]
      
      
      if (!grepl('admssions_by_facility',output_metric)) {
        # Return the number of admissions df w/o the 'ALL' category
        res_df <-
          admissions_by_facility[facility != 'All' &
                                   !grepl('total', grouping),][, facility := gsub("'", "\'", facility)]
      } else {
        res_df <-
          admissions_by_facility[facility == 'All' &
                                   !grepl('total', grouping),][, facility := gsub("'", "\'", facility)]
      }
      true_col = 'hccis_admissions'
      pred_col = 'admissions'
      results_dict <- list(MSE = MSE(as.numeric(res_df$admissions),res_df$hccis_admissions),
                           RMSE = sqrt(MSE(as.numeric(res_df$admissions),res_df$hccis_admissions)),
                           MAE = MAE(as.numeric(res_df$admissions),res_df$hccis_admissions),
                           RAE = RAE(as.numeric(res_df$admissions),res_df$hccis_admissions))
      
    } else if (grepl(
      'treatment_delay',
      output_metric
    )) {
      res_df <-
        results_list[[1]][, treatment_delay := total_wait_time + Travel.time][, .(
          `Coordination Time` = one.boot(
            total_wait_time,
            FUN = mean,
            R = 500,
            na.rm = TRUE
          )$t0,
          `Treatment Delay` = one.boot(
            treatment_delay,
            FUN = mean,
            R = 500,
            na.rm = TRUE
          )$t0
        ),
        by = list(replication, type, `Vulnerable Patient` = Age != 'Adult')]
      for (arg in names(arg_list)) {
        res_df[, (arg) := arg_list[arg]]
      }
      saveRDS(res_df, file = file.path(trial_path, 'time_results_by_replication.rds'))
      results_dict <- as.list(res_df[, .(
        `Coordination Time` = mean(`Coordination Time`, na.rm = T),
        `Treatment Delay` = mean(`Treatment Delay`, na.rm = T)
      ),
      by = list(type, `Vulnerable Patient`)][type == 'Transfer' & `Vulnerable Patient` == T][,list(`Coordination Time`,`Treatment Delay`)])
      

    } else if(grepl('coordination_time',output_metric)){
      res_df <-
        results_list[[1]][, .(
          `Coordination Time` = one.boot(
            total_wait_time,
            FUN = median,
            R = 500,
            na.rm = TRUE
          )$t0),
        by = list(replication, type, `Vulnerable Patient` = Age != 'Adult')
        ][val_env$ed_wait_median, true_val := Target, on = c(`Vulnerable Patient` = 'val_group', type = 'type')]
      results_dict <- list(MSE = MSE(as.numeric(res_df$`Coordination Time`),res_df$true_val),
                          RMSE = sqrt(MSE(as.numeric(res_df$`Coordination Time`),res_df$true_val)),
                          MAE = MAE(as.numeric(res_df$`Coordination Time`),res_df$true_val),
                          RAE = RAE(as.numeric(res_df$`Coordination Time`),res_df$true_val))
      
  }
    write.socket(receiver, jsonlite::toJSON(results_dict, auto_unbox = TRUE))
  }