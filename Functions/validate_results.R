validate_results <- function(patients_df,resource_df,just_mayo = T){
  
  list2env(readRDS(
    file = file.path(
      ".",
      "Data",
      "Function Requirements",
      "MH_Network_sim_input_list.rds"
    )
  ), envir = environment())
  
  val_env <- readRDS(file.path(".","Data Analysis","Validation Metric Dataframes.rds"))
  
  if(just_mayo){
    data <-
      patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)
                  ][, day_num := sim_date(3600 * Enter.Timestamp)
                    ][,`:=`(
                      Age = as.factor(Age),
                      replication = as.factor(replication),
                      type = as.factor(type)
                    )]
    
    mayo_ip <- patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester",Transfer.Site)
                           ][, day_num := sim_date(3600 * Enter.Timestamp)
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
                                   ))}(Age))]
    
    resource_df <- resource_df[grepl('Mayo Rochester',resource,ignore.case = T),]
  }else{
    data <-
      patients_df[,day_num := sim_date(3600 * Enter.Timestamp)
                  ][Age = as.factor(Age),
                    replication = as.factor(replication),
                    type = as.factor(type)]
  }
  
  validation_frames <- list() #Collection of all validation data.frames
  
  # Percentage of ED patients_df transferred somewhere else
  validation_frames$`Mayo Transfers Out by Day` <- 
    sim_val_subfunction(
      df = data[type == 'Transfer',.(count = .N), by = list(replication,Age,day_num)
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
  
  # Rates of external transfers into Mayo Clinic inpatient beds
  validation_frames$`Mayo Transfers in by Day` <-
    sim_val_subfunction(
      df = copy(mayo_ip[!grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)]
      )[order(IP.Arrival.Timestamp), day_num := sim_date(IP.Arrival.Timestamp * 3600), by = list(replication)
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
  
  origin_date <- as.POSIXct("2018-12-01 00:00:00", tz = "UTC") - (3600 * 24 * warm_period)
  resource_df <-
    copy(resource_df)[order(replication,time, system)
                      ][,`:=`(date_time = as.datetime(actual_ip_start * 3600,origin_date),
                              prog_day = as.numeric(as.factor(lubridate::date(as.datetime(actual_ip_start * 3600,origin_date))))
                      )
                      ][, `:=`(prev = data.table::shift(system, 1),
                               occupancy = 100 * (server/capacity),
                               Delta = abs(as.numeric(difftime(date_time, data.table::shift(date_time, 1), 
                                                               units = 'hours')))), 
                        by = list(resource, replication)
                        ][, change := system - prev,
                          by = list(resource, replication)
                          ][, prev := NULL
                            ][,`:=`(`ED Patient` = T, `External Transfer` = F)
                              ][grepl('IP_Unit',patient,ignore.case = T),`ED Patient` := F
                                ][!grepl('Mayo Clinic Hospital - Rochester',patient,ignore.case = T) & 
                                    `ED Patient` == T,`External Transfer` := T]
  
  validation_frames$`IP Unit Queueing Metrics` <-
    rbindlist(
      list(
        #Inpatient Unit Occupancy Rate
        copy(resource_df)[!is.na(Delta),
                          .(Target = signif(weighted.mean(occupancy, Delta), digits = 4)),
                          by = list(resource, replication)
                          ][, .(`Simulation Confidence Interval` = t.test(Target, conf.level = .95)$conf.int %>% 
                                  signif(digits = 4) %>% 
                                  {function(x) paste0("(", x[1], ",", x[2], ")")}()), by = resource
                            ][, resource := {Vectorize(swfun)}(resource)
                              ][, Measure := 'Occupancy Rate'],
        
        #Inpatient Unit Arrival Rate
        copy(resource_df)[change > 0,
                          ][, .(Count = sum(change)), by = list(prog_day, resource, replication)
                            ][CJ(
                              prog_day = prog_day,
                              resource = resource,
                              replication = replication,
                              unique = TRUE),on = .(prog_day, resource, replication)
                              ][is.na(Count), Count := 0
                                ][,.(Count = one.boot(Count,mean,500)$t0),by = list(replication,resource)
                                  ][, .(`Simulation Confidence Interval` = t.test(Count, conf.level = .95)$conf.int %>%
                                          signif(digits = 4) %>%
                                          {function(x) paste0("(", x[1], ",", x[2], ")")}()), by = resource
                                    ][, resource := {Vectorize(swfun)}(resource)
                                      ][, Measure := 'Arrival Rate'],
        # Inpatient Unit Length of Stay
        copy(resource_df)[!is.na(actual_ip_start),time := actual_ip_start
                          ][, .(ip_LoS = abs( abs(max(time) - min(time))),
                                resource = resource), by = list(replication,patient)
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
  
  validation_frames$`Arrival Rates by Patient Type` <- #Inpatient Unit Arrival Rate
    copy(resource_df
    )[change > 0,
      ][, .(Count = .N), by = list(prog_day, resource, replication, `External Transfer`, `ED Patient`)
        ][CJ(
          prog_day = prog_day,
          resource = resource,
          replication = replication,
          `External Transfer` = `External Transfer`,
          `ED Patient` = `ED Patient`,
          unique = TRUE
        ),
        on = .(prog_day,
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
  
  # Compare mean simulated total coordination time vs Mayo total ED wait (by placement type)
  validation_frames$`Average ED Patient Wait Times` <-
    data.table(
      pivot_wider(
        # rbindlist(
        #   list(
        #     sim_val_subfunction(
        #       df = data,
        #       val_env_df = val_env$ed_wait_mean,
        #       metric = 'total_wait_time',
        #       val_group = c('Adolescent', 'Child', 'Geriatric'),
        #       val_function = {
        #         function(x)
        #           mean(x, na.rm = T)
        #       }
        #     )[, val_metric := 'Disposition -> Departure'],
        sim_val_subfunction(
          df = data,
          val_env_df = val_env$coord_mean,
          metric = 'total_wait_time',
          val_group =
            c('Adolescent', 'Child', 'Geriatric'),
          val_function = {
            function(x)
              mean(x, na.rm = T)
          }
        )[, val_metric := 'Disposition -> Bed Assignment'],
        #   ),
        #   use.names = T
        # ),
        id_cols = c(`Vulnerable Patient`, type, `Simulation Confidence Interval`),
        names_from = val_metric,
        values_from = c(`Target Value`, `CI contains True Value?`),
        names_glue = "{val_metric} {.value}"
      )
    ) %>% {function(df) df[, c(setdiff(colnames(df), grep('disp', colnames(df), value = T)),
                               sort(grep('disp', colnames(df), value = T))), with = FALSE]}()
  
  validation_frames$`Median ED Patient Wait Time`<-
    data.table(
      pivot_wider(
        # rbindlist(
        #   list(
        #     sim_val_subfunction(
        #       df = data,
        #       val_env_df = val_env$ed_wait_median,
        #       metric = 'total_wait_time',
        #       val_group =
        #         c('Adolescent', 'Child', 'Geriatric'),
        #       val_function = {
        #         function(x)
        #           median(x, na.rm = T)
        #       }
        #     )[, val_metric := 'Disposition -> Departure'],
        sim_val_subfunction(
          df = data,
          val_env_df = val_env$coord_median,
          metric = 'total_wait_time',
          val_group =
            c('Adolescent', 'Child', 'Geriatric'),
          val_function = {
            function(x)
              median(x, na.rm = T)
          }
        )[, val_metric := 'Disposition -> Bed Assignment'],
        #   ),
        #   use.names = T
        # ),
        id_cols = c(`Vulnerable Patient`, type, `Simulation Confidence Interval`),
        names_from = val_metric,
        values_from = c(`Target Value`, `CI contains True Value?`),
        names_glue = "{val_metric} {.value}"
      )
    ) %>% {function(df) df[, c(setdiff(colnames(df), grep('disp', colnames(df), value = T)),
                               sort(grep('disp', colnames(df), value = T))), with = FALSE]}()
  validation_frames$`Average ED Patient Wait Times` <-
    validation_frames$`Average ED Patient Wait Times`[, `:=`(
      `Delta(Disp -> Assign)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                          true_val = `Disposition -> Bed Assignment Target Value`,
                                                          differences = T)
      # `Delta(Disp -> Departure)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
      #                                               true_val = `Disposition -> Departure Target Value`,
      #                                               differences = T)
    )
    ][, `:=`(`% Error(Disp -> Assignment)` = `Delta(Disp -> Assign)` /  `Disposition -> Bed Assignment Target Value` * 100
             # `% Error(Disp -> Departure)` = `Delta(Disp -> Departure)` / `Disposition -> Departure Target Value` * 100
    )]
  
  validation_frames$`Median ED Patient Wait Time` <-
    validation_frames$`Median ED Patient Wait Time`[, `:=`(
      `Delta(Disp -> Assign)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                          true_val =  `Disposition -> Bed Assignment Target Value`,
                                                          differences = T)
      # `Delta(Disp -> Departure)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
      #                                               true_val = `Disposition -> Departure Target Value`,
      #                                               differences = T)
    )
    ][, `:=`(`% Error(Disp -> Assignment)` = `Delta(Disp -> Assign)` / `Disposition -> Bed Assignment Target Value` * 100
             # `% Error(Disp -> Departure)` = `Delta(Disp -> Departure)` / `Disposition -> Departure Target Value` * 100
    )]
  
  # Average adult number of transfer request rejections
  validation_frames$n_rejects <-
    sim_val_subfunction(
      df = copy(data)[type == 'Transfer', ],
      val_env_df = val_env$rejects,
      metric = 'Times.Rejected',
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
  
  # validation_frames$non_placed_rate <- patients_df[,.(no_placement = sum(as.numeric(is.na(coord_time))),Count = .N), by = list(Age,replication)
  #                                               ][,Rate := 100 * no_placement/Count
  #                                                 ][!is.na(replication),setorderv(.SD,c('Age','replication'))
  #                                                   ][,.(Non_Placement_Rate = list(signif(boot_confInt_val(Rate,conf.level = 0.95)$conf.int,3))) ,by = list(Age)]
  
  # patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester", Transfer.Site),
  #             ][order(Enter.Timestamp),
  #               ][, `:=`(
  #                  resource = {Vectorize(swfun)}(Age),
  #                  LoS = abs(Finish.Timestamp - IP.Arrival.Timestamp)
  #                  )][, .(Avg = one.boot(na.omit(LoS), mean, 1000)$t0),by = list(replication, resource)
  #                     ][, .(`Simulation Confidence Interval` = t.test(Avg, conf.level = .95)$conf.int %>% signif(digits = 4) %>% 
  #                              {function(x) paste0("(", x[1], ",", x[2], ")")}()),
  #                       by = resource]
  
  return(validation_frames)
  # Code to use up to the 99th percentile of coordination time values: coord_time[coord_time < quantile(coord_time,.99,na.rm = T)]
}