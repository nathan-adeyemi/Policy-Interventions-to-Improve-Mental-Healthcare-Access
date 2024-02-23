validate_results <- function(patients_df,
                             resource_df,
                             conf = 0.95,
                             warmup = 50){
  
  list2env(readRDS(
    file = file.path(
      "simulations",
      "function_requirements",
      "MH_Network_sim_input_list.rds"
    )
  ), envir = environment())
  
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
  
  resource_df <-
    resource_df[grepl('Mayo Rochester', resource, ignore.case = T),
                ][order(replication, time, system)
                  ][, `:=`(date_time = as.datetime(time * 3600, origin_date),
                           day_num = as.numeric(as.factor(lubridate::date(as.datetime(time * 3600, origin_date))))),
                    by = replication
                    ][day_num >= warmup
                      ][, `:=`(occupancy = 100 * (server/capacity),
                               Delta = abs(as.numeric(difftime(date_time, data.table::shift(date_time, 1), 
                                                             units = 'hours')))), 
                      by = list(resource, replication)]

  
  validation_frames <- list() #Collection of all validation data.frames
  
  # Percentage of ED patients_df transferred somewhere else
  validation_frames$`Mayo Transfers Out by Day` <- 
    sim_val_subfunction(confidence_level = conf,
      df = mayo_ed_entries[type == 'Transfer',.(count = .N), by = list(replication,Age,day_num)
                ][CJ(day_num = seq(max(day_num,na.rm = TRUE)),
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
      use_type = FALSE)[, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                             true_val = `Target Value`,
                                                             differences = T)
                        ][,`% Error` := Delta/`Target Value` * 100]

  #### Rates of external transfers into Mayo Clinic inpatient beds ####
  validation_frames$`Mayo Transfers in by Day` <-
    sim_val_subfunction(confidence_level = conf,
      df = copy(mayo_ip[!grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)]
      )[order(IP.Arrival.Timestamp), 
        ][, .(count = .N), by = list(Age, replication, day_num)
          ][CJ(day_num = seq(min(day_num, na.rm = T),max(day_num, na.rm = T)),
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
    )[, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                           true_val = `Target Value`,
                                           differences = T)
      ][,`% Error` := Delta/`Target Value` * 100]
  
  
  validation_frames$`IP Unit Queueing Metrics` <-
    rbindlist(
      list(
        #### Inpatient Unit Occupancy Rate ####
        copy(resource_df)[!is.na(Delta),
                          .(Target = signif(weighted.mean(occupancy, Delta), digits = 4)),
                          by = list(resource, replication)
                          ][, .(`Simulation Confidence Interval` = t.test(Target, conf.level = .95)$conf.int %>% 
                                  signif(digits = 4) %>% 
                                  {function(x) paste0("(", x[1], ",", x[2], ")")}()), by = resource
                            ][, resource := {Vectorize(swfun)}(resource)
                              ][, Measure := 'Occupancy Rate'],
        
        #### Inpatient Unit Total Arrival Rate ####
        copy(resource_df)[cap_change > 0,
                          ][, .(Count = sum(cap_change)), by = list(day_num, resource, replication)
                            ][CJ(
                              day_num = day_num,
                              resource = resource,
                              replication = replication,
                              unique = TRUE),on = .(day_num, resource, replication)
                              ][is.na(Count), Count := 0
                                ][,.(Count = one.boot(Count,mean,500)$t0),by = list(replication,resource)
                                  ][, .(`Simulation Confidence Interval` = t.test(Count, conf.level = .95)$conf.int %>%
                                          signif(digits = 4) %>%
                                          {function(x) paste0("(", x[1], ",", x[2], ")")}()), by = resource
                                    ][, resource := {Vectorize(swfun)}(resource)
                                      ][, Measure := 'Arrival Rate'],
        #### Inpatient Unit Length of Stay ####
        unique(copy(resource_df)[,.SD[.N==2],by = list(replication,patient)
                          ][,.(start = min(time), 
                               end = max(time),
                               resource = resource), by = list(replication,patient)
                            ])[, ip_LoS := abs(end - start), by = list(replication,patient)
                            ][, .(ip_LoS = one.boot(ip_LoS, mean, 500, na.rm = T)$t0), by = list(resource,replication)
                              ][,.(`Simulation Confidence Interval` = t.test(ip_LoS,conf.level = .95)$conf.int %>% 
                                     signif(digits = 4) %>% 
                                     {function(x) paste0("(", x[1], ",", x[2], ")")}()),by = resource
                                ][, Measure := "Length of Stay"
                                  ][,resource := {Vectorize(swfun)}(resource)]),
      use.names = T
    )[val_env$ip_unit_metrics, Target := Target, on = c(Measure = 'variable', resource = 'location_description')
      ][,`CI contains True Value?` := {Vectorize(validate_fun)}(`Simulation Confidence Interval`,Target)
        ][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                               true_val = Target,
                                               differences = T)]
  #### Inpatient Arrival Rate by type ####
  validation_frames$`Arrival Rates by Patient Type` <- 
    copy(resource_df
    )[cap_change > 0,
      ][, .(Count = .N), by = list(day_num, resource, replication, `External Transfer`, `ED Patient`)
        ][CJ(
          day_num = day_num,
          resource = resource,
          replication = replication,
          `External Transfer` = `External Transfer`,
          `ED Patient` = `ED Patient`,
          unique = TRUE
        ),
        on = .(day_num,
               resource,
               replication,
               `ED Patient`,
               `External Transfer`)
        ][is.na(Count), Count := 0
          ][, .(Count = one.boot(Count, mean, 500)$t0),
            by = list(replication,
                      resource,
                      `External Transfer`, 
                      `ED Patient`)
            ][, .(`Simulation Confidence Interval` = t.test(Count, conf.level = .95)$conf.int %>% 
                    signif(digits = 4) %>%
                    {function(x) paste0("(", x[1], ",", x[2], ")")}()),
              by = list(resource, `External Transfer`, `ED Patient`)
              ][, resource := {Vectorize(swfun)}(resource)
                ][, variable := 'Arrival Rate'
                  ][val_env$split_arrival_rates, `Target Value` := as.numeric(Target), 
                    on = c(resource = 'val_group',
                           `External Transfer` = 'transfer', 
                           `ED Patient` = 'ed_patient')
                    ][!is.na(`Target Value`),
                      ][, `CI contains True Value?` := {Vectorize(validate_fun)}(`Simulation Confidence Interval`, `Target Value`)
                        ][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                               true_val = `Target Value`,
                                                               differences = T)
                          ][,`% Error` := Delta/`Target Value` * 100
                            ][,setnames(.SD,'resource','IP Unit')]
  
  #### Mean Boarding Period ####
  validation_frames$`Average ED Patient Wait Times` <-
    data.table(
      pivot_wider(
        sim_val_subfunction(
          confidence_level = conf,
          df = boarding_patients,
          val_env_df = val_env$ed_wait_mean,
          metric = 'total_wait_time',
          val_group = c('Adolescent', 'Child', 'Geriatric'),
          val_function = {
            function(x)
              mean(x, na.rm = T)
          },
          include_outliers = FALSE
        )[, val_metric := 'boarding_time'],
        id_cols = c(`Vulnerable Patient`, type, `Simulation Confidence Interval`),
        names_from = val_metric,
        values_from = c(`Target Value`, `CI contains True Value?`),
        names_glue = "{val_metric} {.value}"
      )
    ) %>% {
      function(df)
        df[, c(setdiff(colnames(df), grep('disp', colnames(df), value = T)),
               sort(grep('disp', colnames(df), value = T))), with = FALSE]
    }()
  
  #### Median Boarding Period ####
  validation_frames$`Median ED Patient Wait Time` <-
    data.table(
      pivot_wider(
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
        )[, val_metric := 'boarding_time'],
        id_cols = c(`Vulnerable Patient`, type, `Simulation Confidence Interval`),
        names_from = val_metric,
        values_from = c(`Target Value`, `CI contains True Value?`),
        names_glue = "{val_metric} {.value}"
      )
    ) %>% {
      function(df)
        df[, c(setdiff(colnames(df), grep('disp', colnames(df), value = T)),
               sort(grep('disp', colnames(df), value = T))), with = FALSE]
    }()
  
  
  # Calculate percent error for boarding metrics
  validation_frames$`Average ED Patient Wait Times` <-
    validation_frames$`Average ED Patient Wait Times`[, `:=`(

      `Delta(boarding_time)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                    true_val = `boarding_time Target Value`,
                                                    differences = T)
    )
    ][, `:=`(
      `% Error(boarding_time)` = `Delta(boarding_time)` / `boarding_time Target Value` * 100
    )]
  
  validation_frames$`Median ED Patient Wait Time` <-
    validation_frames$`Median ED Patient Wait Time`[, `:=`(
      `Delta(boarding_time)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                    true_val = `boarding_time Target Value`,
                                                    differences = T)
    )
    ][, `:=`(
     `% Error(boarding_time)` = `Delta(boarding_time)` / `boarding_time Target Value` * 100
    )]
  
  # Average adult number of transfer request rejections
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
      use_type = TRUE
    )[type == 'Transfer'
      ][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                             true_val = `Target Value`,
                                             differences = T)
        ][,`% Error` := Delta/`Target Value` * 100]

  
  return(validation_frames)
}