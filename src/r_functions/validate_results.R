validate_results <- function(patients_df,
                             resource_df,
                             conf = 0.95,
                             warmup = 50,
                             sim_days = 365,
                             fac_admits_only = FALSE,
                             results_by_rep = FALSE) {
  list2env(readRDS(
    file = file.path(
      "src/simulations",
      "function_requirements",
      "MH_Network_sim_input_list.rds"
    )
  ), envir = environment())
  
  normalize <- function(x, ...) {
    return((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  }
  
  resource_val_sub_fn <- function(df,
                                  metric,
                                  metric_name,
                                  conf_level,
                                  val_df_name,
                                  df_splits = 'resource',
                                  val_df_splits = 'location_description') {
    t_test_w_constants <- function(x, conf.level = 0.95) {
      tryCatch(
        expr = {
          result <- t.test(x = x, conf.level = conf.level)
        },
        error = function(e) {
          if (grepl('data are essentially constant', e)) {
            result <- list(conf.int = rep(mean(x, na.rm = TRUE), 2))
          } else {
            cat('An error has occured')
            print(e)
          }
        }
      )
    }
    
    returned_df <- lapply(
      X = conf_level,
      FUN = function(conf) {
        eval(parse(
          text = paste0(
            "df <- df[, .(`",
            conf_level_name,
            "` = paste0('(',paste0(signif(t_test_w_constants(",
            metric,
            ", conf.level = ",
            conf,
            ")$conf.int, digits = 4),collapse = ','),')'),
                      Measure = '",
            metric_name,
            "'), by = list(",
            paste0(df_splits, collapse = ','),
            ")
              ][,resource := {Vectorize(swfun)}(resource)
                ][",
            val_df_name,
            ", Target := Target, on = c(Measure = 'variable',",
            paste(.mapply(
              paste, list(df_splits, val_df_splits), list(sep = ' = ')
            ), collapse = ', '),
            ")
                  ][,`",
            conf_level_name,
            "_contains_target` := {Vectorize(validate_fun)}(`",
            conf_level_name,
            "`,Target)
                    ][, `",
            conf_level_name,
            "_delta` := {Vectorize(validate_fun)}(text = `",
            conf_level_name,
            "`,
                                                           true_val = Target,
                                                           differences = T)]"
          )
        ))
        return(df)
      }
    )
    if (length(conf_level) == 1) {
      returned_df <- returned_df[[1]]
    } else {
      returned_df <- do.call(cbind, returned_df)
    }
    unique_cols = unique(names(returned_df))
    unique_cols <-
      c(setdiff(unique_cols, sort(unlist(
        lapply((conf_level * 100),
               grep,
               x = unique_cols,
               value = T)
      ))), sort(unlist(
        lapply((conf_level * 100),
               grep,
               x = unique_cols,
               value = T)
      )))
    returned_df <- unique(returned_df[, ..unique_cols])
    return(returned_df)
  }
  
  conf_level_name = paste0(100 * conf, '_CI')
  val_env <-
    readRDS(file.path("Data", "Validation Metric Dataframes.rds"))
  hccis_melt <-
    melt(
      copy(hccis)[, rpt_year := NULL],
      id.vars = c('hccis_id', 'owner_hccis_id', 'urban'),
      variable.name = "grouping",
      value.name = 'Target'
    )[, grouping := tolower(grouping)]
  hccis_melt <-
    rbind(hccis_melt, hccis_melt[, .(Target = sum(Target)), by = grouping][grepl('admissions', grouping) &
                                                                             !grepl('icu|ed_to_ip', grouping)][, hccis_id := 'All'], fill = TRUE)[, Target := Target *
                                                                                                                                                    (sim_days / 365)][grepl("adult_admissions|pediatric_admissions|total_admissions",
                                                                                                                                                                            grouping)]
  val_env$facility_admissions <- hccis_melt
  val_env = rbindlist(lapply(names(val_env), function(name)
    val_env[[name]][, df_name := name]), fill = T)[, `:=`(
      log_target = log(Target),
      scale_target = scale(Target),
      norm_target = normalize(Target)
    )]
  
  setDT(patients_df)
  setDT(resource_df)
  
  origin_date <-
    as.POSIXct("2018-12-01 00:00:00", tz = "UTC") + (3600 * 24 * warmup)
  
  boarding_patients <- patients_df[grepl('Mayo', Site.Entered)]
  
  mayo_ed_entries <-
    patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)][, `:=`(
      Age = as.factor(Age),
      replication = as.factor(replication),
      type = as.factor(type)
    )][, `:=`(day_num = sim_date(3600 * Enter.Timestamp)), by = replication][day_num >= warmup, ]
  
  mayo_ip <-
    patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester", Transfer.Site)][, `:=`(
      Age = as.factor(Age),
      replication = as.factor(replication),
      type = as.factor(type),
      unit = {
        # Vectorize(function(i)
        #   switch(
        #     as.character(i),
        #     'Adult' = 'Mayo Rochester Adult',
        #     'Adolescent' = 'Mayo Rochester Pediatric/Adolescent',
        #     'Child' = 'Mayo Rochester Pediatric/Adolescent',
        #     'Geriatric' = 'Mayo Rochester Geriatric'
        #   ))
        Vectorize(swfun)
      }(Age)
    )][, `:=`(day_num = sim_date(3600 * Enter.Timestamp)), by = replication][day_num >= warmup, ]
  
  resource_df <-
    resource_df[is.na(`External Transfer`), `External Transfer` := FALSE][is.na(`ED Patient`), `ED Patient` := FALSE][order(replication, time, system)][, `:=`(date_time = as.datetime(time * 3600, origin_date),
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
  
  mayo_resources <-
    resource_df[grepl('Mayo Rochester', resource, ignore.case = T), ]
  
  if (!fac_admits_only) {
    validation_frames <-
      list() #Collection of all validation data.frames
    # Percentage of ED patients_df transferred somewhere else
    temp <-
      sim_val_subfunction(
        confidence_level = conf,
        df = mayo_ed_entries[type == 'Transfer', .(count = .N), by = list(replication, Age, day_num)][CJ(
          day_num = seq(warmup, (warmup + sim_days)),
          replication = replication,
          Age = Age,
          unique = TRUE
        ),
        on = .(Age, replication, day_num)][!(is.na(day_num)),][is.na(count), count := 0][order(day_num)],
        val_env_df = val_env[df_name == 'transfer_out_rate'],
        metric = 'count',
        val_group = c('Adolescent', 'Child', 'Geriatric'),
        val_function = {
          function(x)
            mean(x, na.rm = T)
        },
        use_type = FALSE,
        results_by_rep = results_by_rep
      )
    
    if (results_by_rep) {
      temp <- temp[, validation_frame := 'transfer_out_rate']
    }
    validation_frames$`Mayo Transfers Out by Day` <- temp
    
    
    #### Rates of external transfers into Mayo Clinic inpatient beds ####
    temp <-
      sim_val_subfunction(
        confidence_level = conf,
        df = copy(mayo_ip[!grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)])[order(IP.Arrival.Timestamp),][, .(count = .N), by = list(Age, replication, day_num)][CJ(
          day_num = seq(warmup, (warmup + sim_days)),
          replication = replication,
          Age = Age,
          unique = TRUE
        ), on = .(Age, replication, day_num)][is.na(count), count := 0],
        val_env_df = val_env[df_name == "Transfer_to_Mayo"],
        metric = 'count',
        val_group = c('Adolescent', 'Child', 'Geriatric'),
        val_function = {
          function(x)
            mean(x, na.rm = T)
        },
        use_type = FALSE,
        results_by_rep = results_by_rep
      )
    
    if (results_by_rep) {
      temp <- temp[, validation_frame := 'Transfer_to_Mayo']
    }
    validation_frames$`Mayo Transfers in by Day` <- temp
    
    #### Inpatient Unit Occupancy Rate ####
    temp <-
      copy(mayo_resources)[!is.na(Delta), .(Target = signif(weighted.mean(occupancy, Delta), digits = 4)), by = list(resource, replication)]
    if (results_by_rep) {
      temp <-
        temp[, resource := {
          Vectorize(swfun)
        }(resource)][, `:=`(
          validation_frame = 'mayo_occupancy_rate',
          sim_val = Target,
          Target = NULL
        )][val_env[df_name == "ip_unit_metrics" &
                     variable == 'Occupancy Rate'], target := Target, on = c(resource = 'location_description')]
    } else {
      temp <- resource_val_sub_fn(
        df = temp,
        conf_level = conf,
        metric = "Target",
        metric_name = "Occupancy Rate",
        val_df_name = 'val_env[df_name == \"ip_unit_metrics\"]',
        df_splits = 'resource',
        val_df_splits = "'location_description'"
      )
    }
    ip_util_val_df <- temp
    
    #### Inpatient Unit Total Arrival Rate ####
    temp <-
      copy(mayo_resources)[cap_change > 0,][, .(Count = sum(cap_change)), by = list(day_num, resource, replication)][CJ(
        day_num = seq(warmup, (warmup + sim_days)),
        resource = resource,
        replication = replication,
        unique = TRUE
      ), on = .(day_num, resource, replication)][is.na(Count), Count := 0][, .(Count = one.boot(Count, mean, 500)$t0), by = list(replication, resource)]
    
    if (results_by_rep) {
      temp <- temp[, resource := {
        Vectorize(swfun)
      }(resource)][, `:=`(
        validation_frame = 'mayo_ip_arr_rates',
        sim_val = Count,
        Count = NULL
      )][val_env[df_name == "ip_unit_metrics" &
                   variable == 'Arrival Rate'],
         target := Target, on = c(resource = 'location_description')]
    } else {
      temp <- resource_val_sub_fn(
        df = temp,
        conf_level = conf,
        metric = "Count",
        metric_name = "Arrival Rate",
        val_df_name = 'val_env[df_name == \"ip_unit_metrics\"]',
        df_splits = 'resource',
        val_df_splits = "'location_description'"
      )
    }
    
    arr_rate_val_df <- temp
    
    #### Inpatient Unit Length of Stay ####
    temp <- unique(copy(mayo_resources)[, .SD[.N == 2], by = list(replication, patient)][, .(ip_LoS = unique(ip_LoS[!is.na(ip_LoS)]),
                                                                                             resource = resource),
                                                                                         by = list(replication, patient)])[, .(ip_LoS = one.boot(ip_LoS, mean, 500, na.rm = T)$t0),
                                                                                                                           by = list(resource, replication)]
    
    if (results_by_rep) {
      temp <- temp[, resource := {
        Vectorize(swfun)
      }(resource)][, `:=`(
        validation_frame = 'mayo_length_of_stay',
        sim_val = ip_LoS,
        ip_LoS = NULL
      )][val_env[df_name == "ip_unit_metrics" &
                   variable == 'Length of Stay'],
         target := Target, on = c(resource = 'location_description')]
    } else{
      temp <- resource_val_sub_fn(
        df = temp,
        conf_level = conf,
        metric = "ip_LoS",
        metric_name =  "Length of Stay",
        val_df_name = 'val_env[df_name == \"ip_unit_metrics\"]',
        df_splits = 'resource',
        val_df_splits = "'location_description'"
      )
    }
    los_val_df <- temp
    
    if (!results_by_rep) {
      validation_frames$`IP Unit Queueing Metrics` <-
        rbindlist(
          list(ip_util_val_df, arr_rate_val_df, los_val_df),
          use.names = T,
          fill = T
        )
    } else {
      validation_frames <- c(
        validation_frames,
        list(ip_util_val_df),
        list(arr_rate_val_df),
        list(los_val_df)
      )
    }
    
    if (!results_by_rep) {
      #### Mean Boarding Period ####
      temp <- sim_val_subfunction(
        confidence_level = conf,
        df = boarding_patients,
        val_env_df = val_env[df_name == "ed_wait_mean"],
        metric = 'total_wait_time',
        val_group = c('Adolescent', 'Child', 'Geriatric'),
        val_function = {
          function(x)
            mean(x, na.rm = T)
        },
        include_outliers = TRUE,
        results_by_rep = results_by_rep
      )
      
      validation_frames$`Average ED Patient Wait Times` <- temp
    }
    #### Median Boarding Period ####
    temp <-
      sim_val_subfunction(
        confidence_level = conf,
        df = boarding_patients,
        val_env_df = val_env[df_name == "ed_wait_median"],
        metric = 'total_wait_time',
        val_group =
          c('Adolescent', 'Child', 'Geriatric'),
        val_function = {
          function(x)
            median(x, na.rm = T)
        },
        include_outliers = FALSE,
        results_by_rep = results_by_rep
      )
    if (results_by_rep) {
      temp <- temp[, validation_frame := 'median_coordination_time']
    }
    validation_frames$`Median ED Patient Wait Time` <- temp
    
    #### Inpatient Arrival Rate by type ####
    temp <- copy(mayo_resources)[cap_change > 0, ][, .(Count = .N),
                                                   by = list(day_num,
                                                             resource,
                                                             replication,
                                                             `External Transfer`,
                                                             `ED Patient`)][CJ(
                                                               day_num = seq(warmup, (warmup + sim_days)),
                                                               resource = resource,
                                                               replication = replication,
                                                               `External Transfer` = `External Transfer`,
                                                               `ED Patient` = `ED Patient`,
                                                               unique = TRUE
                                                             ), on = .(day_num,
                                                                       resource,
                                                                       replication,
                                                                       `ED Patient`,
                                                                       `External Transfer`)][!(`ED Patient` == F &
                                                                                                 `External Transfer` == T), ][is.na(Count), Count := 0][, .(Count = one.boot(Count, mean, 500)$t0),
                                                                                                                                                        by = list(replication,
                                                                                                                                                                  resource,
                                                                                                                                                                  `External Transfer`,
                                                                                                                                                                  `ED Patient`)]
    if (results_by_rep) {
      temp <- temp[, resource := {
        Vectorize(swfun)
      }(resource)][, `:=`(
        validation_frame = 'mayo_split_arr_rates',
        sim_val = Count,
        Count = NULL
      )][val_env[df_name == "split_arrival_rates"],
         target := Target,
         on = c(
           resource = 'location_description',
           `External Transfer` = 'transfer',
           `ED Patient` = 'ed_patient'
         )]
    } else {
      temp <- resource_val_sub_fn(
        df = temp,
        metric = 'Count',
        metric_name = 'Arrival Rate',
        val_df_name = "val_env[df_name == \"split_arrival_rates\"]",
        df_splits = c('resource', '`External Transfer`', '`ED Patient`'),
        val_df_splits = c("'location_description'", "'transfer'", "'ed_patient'"),
        conf_level = conf
      )
    }
    validation_frames$`Arrival Rates by Patient Type` <- temp
    
    temp <-
      sim_val_subfunction(
        confidence_level = conf,
        df = copy(mayo_ed_entries)[type == 'Transfer',],
        val_env_df = val_env[df_name == "rejects"][, type := 'Transfer'],
        metric = 'Rejections',
        val_group = c('Adolescent', 'Child', 'Geriatric'),
        val_function = {
          function(x)
            mean(x, na.rm = T)
        },
        use_type = FALSE,
        results_by_rep = results_by_rep
      )
    if (results_by_rep) {
      temp <- temp[, validation_frame := 'rejections']
    }
    validation_frames$n_rejects <- temp
  }
  
  # Calculate the Average # of Admissions at hospitals ----------------------
  # NEed to add in counting Admissions by Adults/Child and comparing values to HCCIS
  
  facility_admissions_df <-
    copy(resource_df)[cap_change > 0 &
                        is.na(patient) &
                        day_num > warmup, patient := generate_random_id(13)][cap_change > 0 &
                                                                               day_num > warmup,][siteInfo, facility := Facility_name, on = c('resource' = 'Bed_Group')][, is_Adult := Age %in% c('Adult', 'Geriatric')][, .(sim_val = length(unique(patient))), by = list(replication, facility, is_Adult)][is.na(sim_val), sim_val := 0][, `:=`(grouping = as.character(
                                                                                 ifelse(is_Adult, "adult_admissions", "pediatric_admissions")
                                                                               ), is_Adult = NULL)]
  
  total_admission_df <-
    copy(resource_df)[cap_change > 0 &
                        is.na(patient) &
                        day_num > warmup, patient := generate_random_id(13)][cap_change > 0 &
                                                                               day_num > warmup,][siteInfo, facility := Facility_name, on = c('resource' = 'Bed_Group')][, is_Adult := Age %in% c('Adult', 'Geriatric')][, .(sim_val = length(unique(patient))), by = list(replication, facility, is_Adult)][is.na(sim_val), sim_val := 0][, .(facility = 'All', sim_val = sum(sim_val)), by = list(is_Adult, replication)][, `:=`(grouping = as.character(
                                                                                 ifelse(is_Adult, "adult_admissions", "pediatric_admissions")
                                                                               ), is_Adult = NULL)]
  
  facility_admissions_df <-
    rbind(
      facility_admissions_df,
      melt(
        facility_admissions_df[, .(total_admissions = sum(sim_val)), by = list(replication, facility)],
        id.vars = c('replication', 'facility'),
        variable.name = 'grouping',
        value.name = 'sim_val'
      )
    )
  total_admission_df <-
    rbind(
      total_admission_df,
      melt(
        total_admission_df[, .(total_admissions = sum(sim_val)), by = list(replication, facility)],
        id.vars = c('replication', 'facility'),
        variable.name = 'grouping',
        value.name = 'sim_val'
      )
    )
  if (results_by_rep) {
    admissions_by_facility <-
      rbind(facility_admissions_df[, validation_frame := 'admissions_by_facility'],
            total_admission_df[, validation_frame := 'total_admissions'],
            fill = TRUE)[val_env[df_name == 'facility_admissions'], `:=`(target = Target), on = c("grouping" = "grouping", "facility" = "hccis_id")]
  } else {
    eval(parse(
      text = paste0(
        "admissions_by_facility <- rbind(facility_admissions_df, total_admission_df, fill = TRUE)[,.(sim_val = mean(sim_val),`",
        conf_level_name,
        "` = ci_as_text(sim_val)), by = list(facility,grouping)]
                  admissions_by_facility <- admissions_by_facility[val_env[df_name == \"facility_admissions\"],`:=`(target = Target),on = c(\"grouping\" = \"grouping\", \"facility\" = \"hccis_id\")
                  ][,`",
        conf_level_name,
        "_delta` := {Vectorize(validate_fun)}(text =`",
        conf_level_name,
        "`, true_val = target,differences = T)
                  ]#[,.SD[!duplicated(.SD,by = 'target')],by = 'grouping'][order(facility,grouping)]"
      )
    ))
    
    setcolorder(
      admissions_by_facility,
      c(
        'facility',
        'grouping',
        # 'admissions',
        conf_level_name,
        'target',
        paste0(conf_level_name, "_delta")
      )
    )
  }
  
  if (fac_admits_only) {
    return(admissions_by_facility)
  } else{
    validation_frames$admissions_by_facility <- admissions_by_facility
  }
  
  if (results_by_rep) {
    validation_frames <-
      rbindlist(validation_frames, fill = T, use.names = T)
  }
  return(validation_frames)
}