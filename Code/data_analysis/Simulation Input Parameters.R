# Call script to read in data  files and format\
source(
  file = file.path('Code','read_and_format','mayo_data.R')
)

# ED Arrival Rate Calculations ---------------------------------------------------------
esi_rate = copy(disp_to_dep_data)[!is.na(ESI), ][, .(Count = .N), by = ESI][, total := sum(Count)][, `:=`(Perc = Count / total)] %>% with(., setNames(Perc, ESI))

# Counts of ED Arrivals by Date and time of day used for testing non-stationary arrival rates
ed_test_data =
  copy(disp_to_dep_data[type != "Internal Admit"])[, `:=`(
    day = lubridate::date(ed_arrival),
    year = year(ed_arrival),
    weekday = weekdays(ed_arrival, abbreviate = F)
  )][, `:=`(
    Date = as.Date(ed_arrival),
    time_of_day = {Vectorize(timeOfDay)}(ed_arrival),
    day_number = as.numeric(as.factor(as.Date(ed_arrival)))
  )][, .(N = .N, day_number = unique(day_number)), by = list(Date, time_of_day)]

ed_interarrival_test_data =
  copy(disp_to_dep_data[type != "Internal Admit"])[, `:=`(
    day = lubridate::date(ed_arrival),
    month = lubridate::month(ed_arrival, label = TRUE, abbr = FALSE),
    year = year(ed_arrival),
    weekday = weekdays(ed_arrival, abbreviate = F)
  )][order(ed_arrival)][, `:=`(diff = as.numeric(abs(
    difftime(ed_arrival, data.table::shift(ed_arrival, 1), units = "hours")
  )), timeOfDay = {Vectorize(timeOfDay)}(ed_arrival))]

other_days = with(ed_test_data, CJ(Date, time_of_day, unique = T))[, `:=`(N = 0,
  day_number = as.numeric(as.factor(Date)))]

ed_test_data =
  rbind(ed_test_data, other_days)[, .SD[which.max(N)], by = list(Date, time_of_day)][order(Date)][, `:=`(
    month = lubridate::month(Date, label = T, abbr = F),
    day_of_week = lubridate::wday(Date, label = T, abbr = F)
  )]

ed_tests = lapply(c("day_of_week", "month", "time_of_day"),
  function(param) {
    if (param == "time_of_day") {
      return(summary(with(ed_test_data[, .(N = sum(N)), by = c("Date", param)], aov(
        as.formula(paste0("N ~ ", param))
      ))))
    } else {
      return(summary(with(ed_test_data[, eval(parse(text = paste0(
        ".(N = sum(N), ", param, " = unique(", param, "))"
      ))), by = Date], aov(
        as.formula(paste0("N ~ ", param))
      ))))
    }
  }
)

ed_interarrival_tests = lapply(c("weekday", "month", "timeOfDay"),
  function(param) {
    return(summary(with(
      ed_interarrival_test_data, aov(as.formula(paste0("diff ~ ", param)))
    )))
  })
# Non-ED/Non-Transfer IP Arrival Rate Calculations --------------------------------------
days =
  c("Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday")

# Counts of non-ED/non-Transferred patients by day and time of day for testing non-stationary arrival pattern
ip_test_data = with(
  melt(
    all_ip_admits,
    id.vars = c(
      "event_type",
      "location_description",
      "age",
      "bed",
      "source_general"
    ),
    measure.vars = c("ip_admission_ts", "ip_discharge_ts")
    )[variable == "ip_admission_ts" &
      source_general == "Other",
      ][, `:=`(Date = as.Date(value),
               time_of_day = {Vectorize(timeOfDay)}(value),
               day_number = as.numeric(as.factor(as.Date(value))))],
    CJ(Date = unique(as.Date(value)),
              time_of_day = unique(time_of_day),
              unit = unique(location_description)
       )[melt(all_ip_admits,
              id.vars = c("event_type",
                          "location_description",
                          "age",
                          "bed",
                          "source_general"),
              measure.vars = c("ip_admission_ts", "ip_discharge_ts"))
         [variable == "ip_admission_ts" & source_general == "Other",
           ][, `:=`(Date = as.Date(value),
                    time_of_day = {Vectorize(timeOfDay)}(value),
                    day_number = as.numeric(as.factor(as.Date(value)))
                  )][!is.na(time_of_day),
                     ][, .(N = .N, date = as.Date(value)), by = list(day_number, time_of_day, location_description)
                       ],`:=`(N = N), on = c(Date = "date",
                                  unit = "location_description",
                                  time_of_day = "time_of_day")][, `:=`(
                                    month = lubridate::month(Date, label = T, abbr = F),
                                    day_of_week = lubridate::wday(Date, label = T, abbr = F)
                                  )][is.na(N), N := 0])
ip_interarrival_test_data = melt(copy(unit_dynamics),
                                 id.vars = setdiff(colnames(unit_dynamics), c("start", "exit")),
                                 measure.vars = c("start", "exit")
                                 )[order(value, variable)
                                   ][variable == "start" & ed_patient == F
                                     ][, `:=`(timediff =
                                                as.numeric(difftime(
                                                  time1 = value,
                                                  time2 = data.table::shift(value, 1),
                                                  units = "hours"
                                                )), 
                                   ip_admission_ts = value), 
                            by = location_description][, `:=`(month = lubridate::month(ip_admission_ts, label = T, abbr = F),
                                                            day_of_week = lubridate::wday(ip_admission_ts, label = T, abbr = F),
                                                            time_of_day = {Vectorize(timeOfDay)}(ip_admission_ts),
                                                            value = NULL)]

# Test if there are difference in arrival counts by month, day of week, and time of day
ip_tests = lapply(c("day_of_week", "month", "time_of_day"),
  function(param) {
    if (param == "time_of_day") {
      return(summary(with(ip_test_data[, .(N = sum(N)), by = c("Date", param)], aov(
        as.formula(paste0("N ~ ", param))
      ))))
    } else {
      return(summary(with(ip_test_data[, eval(parse(text = paste0(
        ".(N = sum(N), ", param, " = unique(", param, "))"
      ))), by = Date], aov(
        as.formula(paste0("N ~ ", param))
      ))))
    }
  }
)



# Hourly Arrival Rate for Mayo ED and IP
mayo_arrival_rates =
  rbind(ed_test_data[, .(N = sum(N)), by = day_number][, .(AvgAdmits = mean(N) /
    24)][, `:=`(type = "Daily", unit = "ED")],
  ip_test_data[, .(N = sum(N)), by = Date][, .(AvgAdmits = mean(N) /
    24)][, `:=`(type = "Daily", unit = "IP")])

mayo_arrival_factors =
  rbindlist(list(
    setnames(
      weekday_factors(ip_test_data),
      old = "day_of_week",
      new = "type"
    )[, `:=`(unit = "IP", type = as.character(type))],
    setnames(
      time_of_day_factors(ip_test_data),
      old = "time_of_day",
      new = "type"
    )[, `:=`(unit = "IP", type = as.character(type))],
    setnames(
      weekday_factors(ed_test_data),
      old = "day_of_week",
      new = "type"
    )[, `:=`(unit = "ED", type = as.character(type))],
    setnames(
      time_of_day_factors(ed_test_data),
      old = "time_of_day",
      new = "type"
    )[, `:=`(unit = "ED", type = as.character(type))]
  ))

mayo_interarrival_rates =
  rbindlist(list(
    rbind(
      ed_interarrival_test_data[, .(AvgAdmits = (mean(diff, na.rm = T))^
                                      -1), by = timeOfDay][, type := timeOfDay][, timeOfDay := NULL],
      ed_interarrival_test_data[, .(AvgAdmits = (mean(diff, na.rm = T))^
                                      -1), by = weekday][, type := weekday][, weekday := NULL],
      ed_interarrival_test_data[, .(AvgAdmits = (mean(diff, na.rm = T))^
                                      -1), by = month][, type := month][, month := NULL],
      ed_interarrival_test_data[, .(AvgAdmits = (mean(diff, na.rm = T))^
                                      -1)][, type := "Daily"]
    )[, `:=`(factor = AvgAdmits / .SD[type == "Daily", AvgAdmits], unit = "ED")][, location_description := NA],
    
    rbind(
      ip_interarrival_test_data[ed_patient == F, .(AvgAdmits = (mean(timediff, na.rm = T))^
                                                     -1), by = list(location_description,
                                                                    time_of_day)][, type := time_of_day][, time_of_day := NULL],
      ip_interarrival_test_data[ed_patient == F, .(AvgAdmits = (mean(timediff, na.rm = T))^
                                                     -1), by = list(location_description,
                                                                    day_of_week)][, type := day_of_week][, day_of_week := NULL],
      ip_interarrival_test_data[ed_patient == F, .(AvgAdmits = (mean(timediff, na.rm = T))^
                                                     -1), by = list(location_description, month)][, type := month][, month := NULL],
      ip_interarrival_test_data[ed_patient == F, .(AvgAdmits = (mean(timediff, na.rm = T))^
                                                     -1), by = location_description][, type := "Daily"]
    )[, `:=`(factor = AvgAdmits / .SD[type == "Daily", AvgAdmits], unit = "IP"), by = location_description]
  ), use.names = T)

mayo_interarrival_rates[, location_description :=
  {
    Vectorize(
      function(i) {
        switch(i,
          "Adult Psych" = "Mayo Rochester Adult",
          "Child/Adolescent Psych" = "Mayo Rochester Pediatric/Adolescent",
          "Geriatric/Med Psych" = "Mayo Rochester Geriatric",
          "NA" = NA_character_)
      })
  }(as.character(location_description))]

source(file = file.path("Code",
                        "data_analysis",
                        "Build Siteinfo.R"))

# Frequency of Age Among ED Arrivals ------------------------------------
age_frequency = copy(disp_to_dep_data)[, .(Count = .N), by = age_group][, total := sum(Count)][, `:=`(Perc = Count / total)] %>%
  with(., setNames(Perc, age_group)) %>%
  {
    function(i) i[order(names(i))]
  }()

# Inpatient LoS Emprirical Distribution ------------------------
empirical_dist = rbind(
   copy(all_ip_changes
        )[location_description != "ED" & 
             event_end_date < Sys.Date()
          ][, `:=`(instance = as.numeric(rownames(.SD)),
                   unit_change = rleid(location_description)),
            by = patient_number
             ][,.(
                start = min(event_date),
                exit = max(event_end_date),
                age = unique(age)
             ),by = list(patient_number,
                         event_type,
                         unit_change,
                         location_description)
               ][event_type != "Discharge"][, `:=`(type = "Non-ED Admission", unit_change = NULL)],
   copy(ed_bh_admits)[, list(patient_number,
                             ed_disp_init,
                             ip_unit,
                             ip_admission_ts,
                             ip_discharge_ts,
                             age)
                      ][, type := "ED Admission"],
   use.names = F)[, idx := as.numeric(as.factor(paste(patient_number, type, sep = "_")))
                  ][, `:=`(patient_number = NULL)
                    ][order(start, decreasing = F)
                      ][location_description == "Mood Disorders Unit", location_description := "Adult Psych"
                        ][, .(ip_LoS = sum(as.numeric(difftime(exit, start, units = "hours"))),
                              age = unique(age),
                              location_description = unique(location_description),
                              type = unique(type)), by = idx
                          ][, .SD[1, ], by = idx
                            ][, age := age.classify(age)
                              ][,unit_mean_rate := one.boot(data = remove_outliers(ip_LoS),
                                                       FUN = mean,
                                                       R = 500,
                                                       na.rm = T)$t0, by = location_description
                                ][,age_mean_rate := one.boot(data = remove_outliers(ip_LoS),
                                                              FUN = mean,
                                                              R = 500,
                                                              na.rm = T)$t0, by = age]

# External Hospital Probability of Accepting a Patient -----------------------------
add_info_bed_mentions =
  paste0(
    "capacity|full|no availability|no beds|",
    "no capacity|no (.*) availability|no (.*) available|unavailable|",
    "no (.*) capacity|no (.*) beds|no (.*) bed|at capacity|unable (.*) availability"
  )

overall_acceptance_prob =
  as.numeric(ed_transfers[!is.na(facility_contacted) & 
                            !grepl(add_info_bed_mentions, add_info, ignore.case = T)
                          ][, .(overall = 1 - nrow(.SD[call_outcome == "Patient declined by facility", ]) / 
                                   nrow(.SD[call_outcome %in% c("Patient declined by facility", "Patient accepted by facility")]))])
# Change as Necessary
fac_accept_probs =
  ed_transfers[!is.na(facility_contacted) & !grepl(add_info_bed_mentions, add_info, ignore.case = T),
    .(Rejected = "Patient declined by facility" %in% call_outcome),
    by = list(facility_contacted, patient_number)
  ][, .(Acceptance_Prob = (1 - mean(as.numeric(Rejected))),N = .N), by = list(facility_contacted)]

# Time in between Transfer Requests ---------------------------------------
buffers <- unname(unlist(lapply(split(ed_transfers,by = 'patient_number'), function(df) df[order(event_ts),.(values = diff(event_ts,units = 'hours'))]))/3600)
buffers_params = dist_fit(buffers)

# Internal Admit Review Time ----------------------------------------------
# self_coord_estimates = dist_fit(ed_bh_admits[, disp_to_assign])
self_coord_mean = one.boot(ed_bh_admits[, disp_to_assign], FUN = mean, R = 1000)

self_coord_estimates = dist_fit(data = c(remove_zeros(data = na.omit(
  object = admit_beds[, bed_assign_timespan]
))))

# Calculating facility coordination time means and distributions (individual hospitals)
# coord_times =
#   unique(setorder(ed_transfers[!grepl(paste0(
#     "no availability|no beds|",
#     "no capacity|no (.*) availability|no (.*) available|unavailable|",
#     "no (.*) capacity|no (.*) beds|no (.*) bed|at capacity"),
#   add_info,
#   ignore.case = T
#   ) & !grepl("\\<full\\>", add_info, ignore.case = T), ], event_ts)[, `:=`(
#     t1 = min(event_ts),
#     t2_v1 = max(event_ts),
#     t2_v2 = max(.SD[call_outcome %in% c("Patient declined by facility", "Patient declined by facility"), event_ts])
#   ),
#   by = list(patient_number, facility_contacted)
#   ][, .(review_time = {Vectorize(time_extract}(t1, t2_v1, t2_v2)),
#     by = list(facility_contacted, patient_number)][!is.na(review_time), ])
# 
# fac_coord_times =
#   coord_times[, .(
#     Avg = mean(review_time),
#     Dist = dist_fit(review_time),
#     Count = .N
#   ), by = .(facility_contacted)]

# Calculating facility coordination time means and distributions (individual hospitals)----------

coord_times <- ed_transfers[,.SD[.N > 1], by = list(actual_contact,patient_number)][,.(review_time = difftime(event_ts,data.table::shift(event_ts,type = 'lag'),units = 'hours')),by = list(actual_contact,patient_number)]

fac_coord_times =
  coord_times[, .(Avg = one.boot(data = review_time,
                                 FUN = mean,
                                 R = 500,
                                 na.rm = T)$t0, 
                  Dist = dist_fit(review_time),
                  Count = .N),
              by = .(actual_contact)]
bed_prep_data <- unlist(ed_bh_admits[,.(gap = abs(max(abs(bed_assigned_ts - bed_request_ts),
                                                   abs(ip_admission_ts - bed_assigned_ts),
                                                   abs(ip_admission_ts - bed_request_ts)))/3600), by = patient_number][,gap])


bed_prep_params <- fitdist(as.numeric(bed_prep_data),distr = 'exp')

overall_avg = coord_times[, one.boot(review_time, mean, R = 1000,na.rm = T)$t0]
overall_dist = dist_fit(as.numeric(coord_times[, review_time]))

siteInfo <- siteInfo[hospital_systems,
                     hospital_system := hospital_system,
                     on = c(Facility_name = 'name')
                     ][is.na(hospital_system),hospital_system := Facility_name]


# Assigning Request Review Time by Hospital Systems (when appropriate)
siteInfo <-                                                                                                                                                                                                                                                                                                            
  siteInfo[,lowered_name := tolower(Facility_name)
           ][fac_coord_times, `:=`(Review_Info = as.numeric(Avg)), on = c("hospital_system == actual_contact")
             ][fac_coord_times, `:=`(Review_Params = Dist), on = c("hospital_system == actual_contact")
                 ][,Acceptance_Prob := fac_accept_probs[,Acceptance_Prob][match(siteInfo$Facility_name,fac_accept_probs$facility_contacted)],
                   ][,lowered_name := NULL][is.na(Acceptance_Prob) | 
                                                        Acceptance_Prob == 0 |
                                                        Acceptance_Prob == 1, Acceptance_Prob := overall_acceptance_prob] # Assign the overall probability of acceptance to any facility with an unrealistic p value or and Mayo Clinic
siteInfo <- siteInfo[which(lapply(siteInfo[,Review_Params],length)==0), `:=`(Review_Params = overall_dist,
                                               Review_Info = overall_avg)]
hccis =
  data.table(readRDS(file = file.path("simulations", "function_requirements", "hccis.rds")))

distance.matrix =
  readRDS(file = file.path("simulations",
                           "function_requirements",
                           "Drive Distance Matrix.rds"))
time.matrix =
  readRDS(file = file.path("simulations",
                           "function_requirements",
                           "Drive Time Matrix.rds"))

sim_inputs =
  list(
    "mayo_arrival_rates" = rbindlist(
      list(mayo_arrival_factors, mayo_arrival_rates),
      use.names = T,
      fill = T
    ),
    "mayo_interarrival_rates" = mayo_interarrival_rates,
    "siteInfo" = siteInfo,
    "age_frequency" = age_frequency,
    "hccis" = hccis,
    "empirical_dist" = empirical_dist,
    "bed_prep_params" = bed_prep_params,
    "self_coord_estimates" = self_coord_estimates,
    "distance.matrix" = distance.matrix,
    "time.matrix" = time.matrix,
    "esi_frequency" = esi_rate
  )
saveRDS(
  object = sim_inputs,
  file = file.path(
    "simulations",
    "function_requirements",
    "MH_Network_sim_input_list.rds"
  )
)
saveRDS(
  object = siteInfo,
  file = file.path(
    "simulations",
    "function_requirements",
    "ip_facilities_info.rds"
))

