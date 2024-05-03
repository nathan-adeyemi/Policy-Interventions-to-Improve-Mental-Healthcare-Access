validate_results <- function(patients_df,
                             resource_df,
                             conf = 0.95,
                             warmup = 50,
                             sim_days = 365){

  list2env(readRDS(
    file = file.path(
      "simulations",
      "function_requirements",
      "MH_Network_sim_input_list.rds"
    )
  ), envir = environment())

  resource_val_sub_fn <- function(df,
                                  metric,
                                  metric_name, 
                                  conf_level, 
                                  val_df_name,
                                  df_splits = 'resource',
                                  val_df_splits = 'location_description') {
    
    t_test_w_constants <- function(x, conf.level = 0.95){
      tryCatch(expr = {
        result <- t.test(x = x, conf.level = conf.level)
      },error = function(e){
        if(grepl('data are essentially constant',e)){
          result <- list(conf.int = rep(mean(x,na.rm = TRUE),2))
        } else {
          cat('An error has occured')
          print(e)
        }
      })
    }
    
    ret <- lapply(
        X = conf_level,
        FUN = function(conf) {
          conf_level_name = paste0(100*conf,'_CI')
          eval(parse(
            text = paste0(
              "df <- df[, .(`",conf_level_name,"` = paste0('(',paste0(signif(t_test_w_constants(",metric,", conf.level = ",conf,")$conf.int, digits = 4),collapse = ','),')'),
                      Measure = '",metric_name,"'), by = list(",paste0(df_splits, collapse = ','),")
              ][,resource := {Vectorize(swfun)}(resource)
                ][",val_df_name,", Target := Target, on = c(Measure = 'variable',",paste(.mapply(paste,list(df_splits,val_df_splits),list(sep = ' = ')),collapse = ', '),")
                  ][,`",conf_level_name,"_contains_target` := {Vectorize(validate_fun)}(`",conf_level_name,"`,Target)
                    ][, `",conf_level_name,"_delta` := {Vectorize(validate_fun)}(text = `",conf_level_name,"`,
                                                           true_val = Target,
                                                           differences = T)]"
            )
          ))
          return(df)
        }
      )
    if (length(conf_level) == 1) {
      ret <- ret[[1]]
    } else {
      ret <- do.call(cbind,ret)
    }
    unique_cols = unique(names(ret))
    unique_cols <-
      c(setdiff(unique_cols, sort(unlist(
        lapply((conf_level * 100),
               grep,
               x = unique_cols,
               value = T
        )
      ))), sort(unlist(
        lapply((conf_level * 100),
               grep,
               x = unique_cols,
               value = T
        )
      )))
    ret <- unique(ret[, ..unique_cols])
      return(ret)
  }
  
  
  val_env <- readRDS(file.path("Data","Validation Metric Dataframes.rds"))

  setDT(patients_df)
  setDT(resource_df)


  origin_date <- as.POSIXct("2018-12-01 00:00:00", tz = "UTC") + (3600 * 24 * warmup)

  boarding_patients <- patients_df[grepl('Mayo',Site.Entered)]

  mayo_ed_entries <-
    patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)
                  ][,`:=`(
                    Age = as.factor(Age),
                    replication = as.factor(replication),
                    type = as.factor(type))
                    ][,`:=`(day_num = sim_date(3600 * Enter.Timestamp)),by = replication
                     ][day_num >= warmup,]

  mayo_ip <- patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester",Transfer.Site)
                           ][,`:=`(
                             Age = as.factor(Age),
                             replication = as.factor(replication),
                             type = as.factor(type),
                             unit = {
                               Vectorize(function(i)
                                 switch(
                                   as.character(i),
                                   'Adult' = 'Mayo Rochester Adult',
                                   'Adolescent' = 'Mayo Rochester Pediatric/Adolescent',
                                   'Child' = 'Mayo Rochester Pediatric/Adolescent',
                                   'Geriatric' = 'Mayo Rochester Geriatric'
                                 ))}(Age))
                             ][,`:=`(day_num = sim_date(3600 * Enter.Timestamp)),by = replication
                                ][day_num >= warmup,]
  
  resource_df <-resource_df[is.na(`External Transfer`),`External Transfer` := FALSE
                  ][is.na(`ED Patient`), `ED Patient` := FALSE
                    ][order(replication, time, system)
                      ][, `:=`(date_time = as.datetime(time * 3600, origin_date),
                               day_num = as.numeric(as.factor(lubridate::date(as.datetime(time * 3600, origin_date))))),
                        by = replication
                        ][day_num >= warmup
                          ][, `:=`(occupancy = 100 * (server/capacity),
                                   Delta = abs(as.numeric(difftime(date_time, data.table::shift(date_time, 1),
                                                                 units = 'hours')))),
                          by = list(resource, replication)]
  
  mayo_resources <- resource_df[grepl('Mayo Rochester', resource, ignore.case = T),]


  validation_frames <- list() #Collection of all validation data.frames
  # Percentage of ED patients_df transferred somewhere else
  validation_frames$`Mayo Transfers Out by Day` <-
    sim_val_subfunction(
      confidence_level = conf,
      df = mayo_ed_entries[type == 'Transfer',.(count = .N), by = list(replication,Age,day_num)
                ][CJ(day_num = seq(warmup,(warmup + sim_days)),
                     replication = replication, Age = Age, unique = TRUE),
                  on = .(Age,replication,day_num)
                  ][!(is.na(day_num)),
                    ][is.na(count),count := 0][order(day_num)],
      val_env_df = val_env$transfer_out_rate,
      metric = 'count',
      val_group = c('Adolescent', 'Child', 'Geriatric'),
      val_function = {
        function(x)
          mean(x, na.rm = T)
      },
      use_type = FALSE)

  #### Rates of external transfers into Mayo Clinic inpatient beds ####
  validation_frames$`Mayo Transfers in by Day` <-
    sim_val_subfunction(
      confidence_level = conf,
      df = copy(mayo_ip[!grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)]
      )[order(IP.Arrival.Timestamp),
        ][, .(count = .N), by = list(Age, replication, day_num)
          ][CJ(day_num = seq(warmup, (warmup + sim_days)),
               replication = replication,
               Age = Age,
               unique = TRUE),on = .(Age, replication, day_num)
            ][is.na(count), count := 0],
      val_env_df = val_env$Transfer_to_Mayo,
      metric = 'count',
      val_group = c('Adolescent', 'Child', 'Geriatric'),
      val_function = {
        function(x)
          mean(x, na.rm = T)
      },
      use_type = FALSE
    )

  validation_frames$`IP Unit Queueing Metrics` <-
    rbindlist(
      list(
        #### Inpatient Unit Occupancy Rate ####
        resource_val_sub_fn(
          df = copy(mayo_resources)[!is.na(Delta), .(Target = signif(weighted.mean(occupancy, Delta), digits = 4)), by = list(resource, replication)],
          conf_level = conf,
          metric = "Target",
          metric_name = "Occupancy Rate",
          val_df_name = 'val_env$ip_unit_metrics',
          df_splits = 'resource',
          val_df_splits = "'location_description'"
        ),
        
        #### Inpatient Unit Total Arrival Rate ####
        resource_val_sub_fn(
          df = copy(mayo_resources)[cap_change > 0, ][, .(Count = sum(cap_change)), by = list(day_num, resource, replication)][CJ(
            day_num = seq(warmup,(warmup + sim_days)),
            resource = resource,
            replication = replication,
            unique = TRUE
          ), on = .(day_num, resource, replication)][is.na(Count), Count := 0][, .(Count = one.boot(Count, mean, 500)$t0), by = list(replication, resource)],
          conf_level = conf,
          metric = "Count",
          metric_name = "Arrival Rate",
          val_df_name = 'val_env$ip_unit_metrics',
          df_splits = 'resource',
          val_df_splits = "'location_description'"
        ),
        #### Inpatient Unit Length of Stay ####
        resource_val_sub_fn(
          df = unique(copy(mayo_resources)[, .SD[.N == 2], by = list(replication, patient)
          ][, .(ip_LoS = unique(ip_LoS[!is.na(ip_LoS)]),
                resource = resource), 
            by = list(replication, patient)]
          )[, .(ip_LoS = one.boot(ip_LoS, mean, 500, na.rm = T)$t0),
                        by = list(resource, replication)],
          conf_level = conf,
          metric = "ip_LoS",
          metric_name =  "Length of Stay",
          val_df_name = 'val_env$ip_unit_metrics',
          df_splits = 'resource',
          val_df_splits = "'location_description'"
        )
      ),use.names = T)
      
  #### Mean Boarding Period ####
  validation_frames$`Average ED Patient Wait Times`  <- sim_val_subfunction(
    confidence_level = conf,
    df = boarding_patients,
    val_env_df = val_env$ed_wait_mean,
    metric = 'total_wait_time',
    val_group = c('Adolescent', 'Child', 'Geriatric'),
    val_function = {
      function(x)
        mean(x, na.rm = T)
    },
    include_outliers = TRUE)

  #### Median Boarding Period ####
  validation_frames$`Median ED Patient Wait Time` <-
    sim_val_subfunction(
      confidence_level = conf,
      df = boarding_patients,
      val_env_df = val_env$ed_wait_median,
      metric = 'total_wait_time',
      val_group =
        c('Adolescent', 'Child', 'Geriatric'),
      val_function = {
        function(x)
          median(x, na.rm = T)
      },
      include_outliers = FALSE
    )
  #### Inpatient Arrival Rate by type ####
  validation_frames$`Arrival Rates by Patient Type` <-
    resource_val_sub_fn(df = copy(mayo_resources)[cap_change > 0,][, .(Count = .N),
                                                                by = list(day_num,
                                                                          resource,
                                                                          replication,
                                                                          `External Transfer`,
                                                                          `ED Patient`)][CJ(
                                                                            day_num = seq(warmup,(warmup + sim_days)),
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
                                                                                                              `External Transfer` == T),][is.na(Count), Count := 0][, .(Count = one.boot(Count, mean, 500)$t0),
                                                                                                                                                                    by = list(replication,
                                                                                                                                                                              resource,
                                                                                                                                                                              `External Transfer`,
                                                                                                                                                                              `ED Patient`)],
                        metric = 'Count',
                        metric_name = 'Arrival Rate',
                        val_df_name = "val_env$split_arrival_rates",
                        df_splits = c('resource','`External Transfer`','`ED Patient`'),
                        val_df_splits = c("'location_description'","'transfer'","'ed_patient'"),
                        conf_level = conf)
  
  

# Calculate the Average # of Admissions at hospitals ----------------------
  # NEed to add in counting Admissions by Adults/Child and comparing values to HCCIS
  
  facility_admissions_df <- copy(resource_df)[cap_change > 0 & is.na(patient) & day_num > warmup, patient := generate_random_id(13)
  ][cap_change > 0 & day_num > warmup,
  ][siteInfo, facility := Facility_name, on = c('resource' = 'Bed_Group')
  ][, is_Adult := Age %in% c('Adult', 'Geriatric')
  ][, .(admissions = length(unique(patient))), by = list(replication, facility, is_Adult)
  ][is.na(admissions),admissions := 0
  ][, `:=`(grouping = as.character(ifelse(is_Adult, "adult_admissions", "pediatric_admissions")),is_Adult = NULL)]
  
  total_admission_df <- copy(resource_df)[cap_change > 0 & is.na(patient) & day_num > warmup, patient := generate_random_id(13)
  ][cap_change > 0 & day_num > warmup,
  ][siteInfo, facility := Facility_name, on = c('resource' = 'Bed_Group')
  ][, is_Adult := Age %in% c('Adult', 'Geriatric')
  ][, .(admissions = length(unique(patient))), by = list(replication, facility, is_Adult)
  ][is.na(admissions),admissions := 0
  ][,.(facility = 'All', admissions = sum(admissions)),by = list(is_Adult,replication)
  ][, `:=`(grouping = as.character(ifelse(is_Adult, "adult_admissions", "pediatric_admissions")),is_Adult = NULL)]
  
  facility_admissions_df <- rbind(facility_admissions_df,melt(facility_admissions_df[,.(total_admissions = sum(admissions)), by = list(replication,facility)], id.vars = c('replication','facility'), variable.name = 'grouping',value.name = 'admissions'))
  total_admission_df <- rbind(total_admission_df,melt(total_admission_df[,.(total_admissions = sum(admissions)), by = list(replication,facility)], id.vars = c('replication','facility'), variable.name = 'grouping',value.name = 'admissions'))
  hccis_melt <- melt(copy(hccis)[,rpt_year := NULL], id.vars = c('hccis_id','owner_hccis_id','urban'), variable.name = "grouping",value.name = 'value')[,grouping := tolower(grouping)]
  hccis_melt <- rbind(hccis_melt,hccis_melt[,.(value = sum(value)),by = grouping][grepl('admissions',grouping) & !grepl('icu|ed_to_ip',grouping)][,hccis_id:= 'All'],fill = TRUE)

  admissions_by_facility <- rbind(facility_admissions_df,total_admission_df,fill = TRUE)[,.(sim_CI = ci_as_text(admissions)), by = list(facility,grouping)]
  validation_frames$admissions_by_facility <- admissions_by_facility[hccis_melt,`:=`(admissions = value*(sim_period/365)),on = c("grouping" = "grouping", "facility" = "hccis_id")
                                                   ][,error := {Vectorize(validate_fun)}(text = sim_CI, true_val = admissions,differences = T)
                                                   ][,.SD[!duplicated(.SD,by = 'admissions')],by = 'grouping'][order(facility,grouping)]

  setcolorder(validation_frames$admissions_by_facility, c('facility','grouping','sim_CI','admissions','error'))

  validation_frames$n_rejects <-
    sim_val_subfunction(confidence_level = conf,
                        df = copy(mayo_ed_entries)[type == 'Transfer', ],
                        val_env_df = val_env$rejects[,type := 'Transfer'],
                        metric = 'Rejections',
                        val_group = c('Adolescent', 'Child', 'Geriatric'),
                        val_function = {
                          function(x)
                            mean(x, na.rm = T)
                        },
                        use_type = FALSE
    )

  return(validation_frames)
}