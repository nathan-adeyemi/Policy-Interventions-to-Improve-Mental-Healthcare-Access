# Read in helper datasets facility aliases and what facilities are in hwhich ealth systems) ---------------------------------------
source(
  file = file.path('Code','read_and_format','mayo_data.R')
)

disp_to_dep_data <- disp_to_dep_data[type != 'Internal Admit (all beds set)']
vulnerable_patients <- c('Child','Adolescent','Geriatric')

age_rates <- disp_to_dep_data[,.(Rate = .N/nrow(disp_to_dep_data)),
                              by = list(age_group)]

# Validation and Parameter Estimation Calculations ------------------------------------------

# Extract true value for avergae disposition -> ED departure timespan

# Median Timespan between ED disposition and departure -------------------
ed_wait_median <-
  extract_time_validation_metric(
    data = copy(disp_to_dep_data),
    metric = 'boarding_hours',
    val_group = vulnerable_patients,
    val_func = DescTools::Quantile,
    probs = 0.5,
    na.rm = TRUE
  )

# Mean timespan between ED disposition and departure ----------------------
ed_wait_mean <-
  extract_time_validation_metric(
    data = copy(disp_to_dep_data),
    metric = 'boarding_hours',
    val_group = vulnerable_patients,
    val_func = mean
  )

# Mean # of daily transfers out of Mayo -----------------------------------
transfer_out_rate <-
  extract_time_validation_metric(
    data = disp_to_dep_data[grepl('transfer',type,ignore.case = T)
                            ][,.(count = .N),by = list(age_group,prog_day_I)
                              ][CJ(age_group = age_group,
                                   prog_day_I = seq(from = min(prog_day_I,na.rm = T),
                                                    to = max(prog_day_I,na.rm = T),
                                                    by = 1),
                                   unique = TRUE),
                                on = .(age_group, prog_day_I)
                                ][order(prog_day_I),
                                  ][is.na(count),count := 0],
    metric = 'count',
    val_group = vulnerable_patients,
    val_func =  mean
  )

# Mean # of daily transfers into Mayo -----------------------------------
Transfer_to_Mayo <-
  extract_time_validation_metric(
    data = copy(transferred_admits)[patient_number %in% unique(all_ip_changes$transferred_admits_patient_num) & 
                                       Status == 'Completed' & 
                                       unit %in% c('Geriatric/Med Psych',
                                                  'Child/Adolescent Psych',
                                                  'Adult Psych',
                                                  'Mood Disorders Unit') &
                                      facility_type == 'ED', .(count = .N), by = list(age_group, prog_day_I)
                                    ][CJ(age_group = age_group,
                                         prog_day_I = seq(from = min(prog_day_I,na.rm = T),
                                                          to = max(prog_day_I,na.rm = T),
                                                          by = 1),
                                         unique = TRUE),
                                      on = .(age_group, prog_day_I)
                                      ][is.na(count), count := 0],
    metric = 'count',
    val_group = vulnerable_patients,
    val_func = mean
  )

# Calculating the average number of patient rejections  -------------------
# debug(extract_time_validation_metric)
rejects <-
   extract_time_validation_metric(
      data = unique(ed_transfers[!grep(pattern = "\\b(WI)\\b", ED_name),
                                 .(
                                    patient_number = patient_number,
                                    age = age,
                                    rejections =  rejections,
                                    ED_name = ED_name
                                 )])[, `:=`(age_group = {Vectorize(age_classify)}(age),
                                            type = 'Transfer')],
      metric = 'rejections',
      val_group = vulnerable_patients,
      val_func = mean
   )
# Average # of hospitals contacted per patients ---------------------------
sites_contacted <-
  extract_time_validation_metric(
    data = unique(ed_transfers[!grep(pattern = "\\b(WI)\\b", ED_name),
                               .(age = age,
                                 sites_contacted = length(unique(facility_contacted)),
                                 ED_name = ED_name
                               ),by = patient_number])[, `:=`(age_group = Vectorize(age_classify)(age))],
    metric = 'sites_contacted',
    val_group = vulnerable_patients,
    val_func =  mean
  )

# Average Distance travelled by external transfers ---------------------------
# travel.distance <-
#   extract_time_validation_metric(
#     data = unique(ed_transfers[!grep(pattern = "\\b(WI)\\b", ED_name),
#                                .(age = age,
#                                  travel.distance = max(Travel.Distance),
#                                  ED_name = ED_name
#                                ),by = patient_number]
#                   )[!is.infinite(travel.distance),
#                     ][, `:=`(age_group = Vectorize(age_classify)(age),
#                              type = 'Transfer')],
#     metric = 'travel.distance',
#     val_group = vulnerable_patients,
#     val_func = mean
#   )

# Inpatient Unit QT Metrics -----------------------------------------------
ip_unit_metrics <- 
  rbindlist(list(
               # Inpatient Length of Stay
              copy(unit_dynamics)[start > as.POSIXct("2019-06-04") & !is.na(prog_day_I)
                                  ][,.(Total_LoS = sum(as.numeric(difftime(exit,start,units = 'hours'))),
                                       age =  unique(age),
                                       location_description = unique(location_description),
                                       type = unique(type)),by = idx
                                    ][,.SD[1,],by = idx
                                      ][,.(Target = one.boot(remove_outliers(Total_LoS),{function(i) mean(i,na.rm = T)},500)$t0,
                                         CI = signif(boot.ci(one.boot(remove_outliers(Total_LoS),{function(i) mean(i,na.rm = T)},500),.95,'basic')[['basic']],4) %>%
                                           {function(x) paste0('(',x[4],",",x[5],")")}()),
                                      by = list(location_description)
                                      ][,`:=`(variable = 'Length of Stay',val_group = location_description)] %>%
                relocate('variable', .after = 'location_description'),

              # Utilization
              melt(copy(unit_dynamics)[start > as.POSIXct("2019-06-04") & !is.na(prog_day_I)],
                   id.vars = setdiff(colnames(unit_dynamics),c('start','exit')),
                    measure.vars = c('start','exit')
                   )[,`:=`(event_type = NULL,change = 1)
                    ][variable != 'start', change := -1
                      ][,variable := factor(variable,levels = c('exit','start')), # so paired exit/starts show up correctly
                        ][order(value,decreasing = F)
                          ][,`:=`(Capacity = cumsum(change),
                                  Utilization = signif(100 * (cumsum(change)/max_unit_beds[location_description]),2)),
                            by = location_description
                            ][,`:=`(prog_day = as.numeric(as.factor(lubridate::date(value))))
                              ][order(value,variable)
                                ][value > lubridate::date('2019-01-10') & value < lubridate::date('2022-01-01'),
                                    ][,delta := as.numeric(difftime(value,data.table::shift(value,n = 1),units = 'hours')),
                                      by = location_description
                                      ][!is.na(delta),.(Target = signif(weighted.mean(Utilization,delta),digits = 4),
                                                        CI = weighted_mean_ci(Utilization,delta) %>% signif(digits = 4) %>% ci_as_text()),
                                        by = list(location_description)
                                        ][,`:=`(variable = 'Occupancy Rate',val_group = location_description)] %>% relocate('variable', .after = 'location_description'),
              # Inpatient Arrival Rate
              melt(copy(unit_dynamics)[start > as.POSIXct("2019-06-04") & !is.na(prog_day_I)],
                   id.vars = setdiff(colnames(unit_dynamics),c('start','exit')),
                   measure.vars = c('start','exit'))[order(value,variable)
                                                     ][,`:=`(event_type = NULL,change = 1)
                                                       ][variable != 'start', change := -1
                                                         ][order(value,decreasing = F)
                                                           ][,`:=`(Occupancy = cumsum(change),
                                                                   Utilization = signif(100 * (cumsum(change)/max_unit_beds[location_description]),4)),
                                                             by = location_description
                                                             ][,variable := factor(variable,levels = c('exit','start')), # so paired exit/starts show up correctly
                                                                 ][value > lubridate::date('2019-01-10') & value < lubridate::date('2022-01-01'),
                                                                   ][,.(Day_Count = .N, Occupancy = mean(Utilization)),
                                                                     by = list(prog_day_I,location_description,variable)
                                                                     ][CJ(prog_day_I = seq(max(prog_day_I)), location_description = location_description, variable = variable, unique = TRUE),
                                                                       on = .(prog_day_I,location_description,variable)
                                                                       ][is.na(Day_Count), Day_Count := 0
                                                                         ][,.(Target = one.boot(data = Day_Count,mean,R = 500, na.rm = TRUE)$t0,
                                                                              CI = signif(boot.ci(one.boot(data = Day_Count, FUN = mean, R = 500, na.rm = T),
                                                                                                  .95,'basic')[['basic']][c(4,5)],4) %>% ci_as_text()),
                                                                           by = list(location_description,variable)
                                                                           ][variable == 'start',variable := 'Arrival Rate'
                                                                             ][variable == 'exit',variable := 'Discharge Rate'][,val_group := location_description]))

split_arrival_rates <- melt(
  copy(unit_dynamics[start > as.POSIXct("2019-06-04") & !is.na(prog_day_I)]),
  id.vars = setdiff(colnames(unit_dynamics), c('start', 'exit')),
  measure.vars = c('start', 'exit')
  )[order(value, variable)
    ][, `:=`(event_type = NULL,
               change = 1,
               age = Vectorize(age_classify)(age))
        ][variable != 'start', change := -1
          ][order(value, decreasing = F)
            ][, `:=`(Occupancy = cumsum(change),
                     Utilization = signif(100 * (cumsum(change) /max_unit_beds[location_description]), 4)),
               by = location_description
              ][, variable := factor(variable, levels = c('exit', 'start')), # so paired exit/starts show up correctly][value > lubridate::date('2019-01-10') &][value < lubridate::date('2022-01-01'),
                  ][, .(Day_Count = .N),
                by = list(prog_day_I,
                          location_description,
                          variable,
                          transfer,
                          ed_patient)
                ][CJ(prog_day_I = seq(max(prog_day_I,na.rm = T)),
                  location_description = location_description,
                  variable = variable,
                  transfer = transfer,
                  ed_patient = ed_patient,
                  unique = TRUE), 
                  on = .(prog_day_I,
                         location_description,
                         variable,
                         transfer, 
                         ed_patient)
                  ][is.na(Day_Count), Day_Count := 0
                    ][, .(Target = one.boot(Day_Count,
                                            FUN = {
                                              function(i)
                                                mean(i, na.rm = T)
                                            },
                                            R = 500)$t0),
                      by = list(location_description,
                                variable,
                                transfer,
                                ed_patient)
                      ][variable == 'start', variable := 'Arrival Rate'
                        ][, val_group := location_description
                          ][Target != 0, ][variable == 'Arrival Rate']

rm(list = lsf.str())

saveRDS(list(ed_wait_mean = ed_wait_mean,
             ed_wait_median = ed_wait_median,
             transfer_out_rate = transfer_out_rate,
             Transfer_to_Mayo = Transfer_to_Mayo,
             rejects = rejects,
             ip_unit_metrics = ip_unit_metrics,
             split_arrival_rates = split_arrival_rates),
        file = file.path(".","Data","Validation Metric Dataframes.rds"))
