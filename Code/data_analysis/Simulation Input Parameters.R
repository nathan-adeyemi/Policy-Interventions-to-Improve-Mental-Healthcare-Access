# Call script to read in data  files and format
source(
  file = file.path('Code','read_and_format','mayo_data.R')
)

# Call script to read in data  files and format\
source(
  file = file.path('Code','read_and_format','hccis_data.R')
)

# ED Arrival Rate ---------------------------------------------------------
ed_arrival_rates <-
  copy(disp_to_dep_data[type != "Internal Admit" & ED == 'RST ROMB ED']
       )[, .(N = .N), by = list(Date = as.Date(ed_arrival))
         ][CJ(Date),on = .(Date)
           ][is.na(N), N := 0
             ][,day_of_week := wday(Date, label = T, abbr = F)]

ed_arrival_rates <- rbind(
  # copy(ed_arrival_rates)[, `:=`(type = time_of_day, time_of_day = NULL)][, .(AvgAdmits = mean(N) /6), by = type],
  copy(ed_arrival_rates)[, .(N = sum(N)), by = Date][, .(AvgAdmits = mean(N) /24, type = 'Daily')],
  copy(ed_arrival_rates)[, .(N = sum(N), day_of_week), by = Date][, `:=`(type = day_of_week, day_of_week = NULL)][, .(AvgAdmits = mean(N) / 24), by = type],
  fill = T
)[, factor := AvgAdmits / .SD[type == 'Daily', AvgAdmits]]


# Inpatient (Non ED) Arrival Rates -------------------------------------------------
ip_arrival_rates <- 
  unit_dynamics[,list(unit = location_description_full, Date = start,ed_patient, transfer)
  ][ed_patient == FALSE & transfer == FALSE
   ][,.(N = .N), by = list(unit,Date = as.Date(Date))
     ][CJ(Date = seq(min(Date),max(Date), by = 1),
          unit,
          unique = T, sorted = F), on = .(unit,Date)
       ][,`:=`(prog_day_I = as.numeric(as.factor(as.Date(Date))),
               day_of_week = lubridate::wday(Date,label = T, abbr = F))
         ][is.na(N),N := 0]

ip_arrival_rates <- rbind(
  copy(ip_arrival_rates)[, .(N = sum(N)), by = list(unit, Date)][, .(AvgAdmits = mean(N) /24, type = 'Daily'),  by = unit],
  copy(ip_arrival_rates)[, .(N = sum(N), day_of_week), by = list(unit, Date)][, `:=`(type = day_of_week, day_of_week = NULL)][, .(AvgAdmits = mean(N) / 24), by = list(type, unit)],
  fill = T
)[, factor := AvgAdmits / .SD[type == 'Daily', AvgAdmits], by = unit]

# Frequency of Age Among ED Arrivals ------------------------------------
age_frequency = copy(disp_to_dep_data)[, .(Count = .N), by = age_group][, total := sum(Count)][, `:=`(Perc = Count / total)] %>%
  with(., setNames(Perc, age_group)) %>%
  {
    function(i) i[order(names(i))]
  }()

# Inpatient LoS Empirical Distribution ------------------------
empirical_dist = unit_dynamics[, .(
  ip_LoS = sum(as.numeric(difftime(exit, start, units = "hours"))),
  age = age_classify(age),
  location_description_full = unique(location_description_full),
  type = unique(type)[1]
), by = list(idx, location_description_full)][, .SD[1,], by = list(idx, location_description_full)][, unit_mean_rate := one.boot(
  data = (ip_LoS),
  FUN = mean,
  R = 500,
  na.rm = T
)$t0, by = location_description_full][, age_mean_rate := one.boot(
  data = (ip_LoS),
  FUN = mean,
  R = 500,
  na.rm = T
)$t0, by = age]

# External Hospital Probability of Accepting a Patient -----------------------------
add_info_bed_mentions =
  paste(
    "capacity","full","no availability","no beds",
    "no capacity","no (.*) availability","no (.*) available","unavailable",
    "no (.*) capacity","no (.*) beds","no (.*) bed","at capacity","unable (.*) availability", sep = "|")

overall_acceptance_prob =
  as.numeric(ed_transfers[!is.na(facility_contacted) &
                            !grepl(add_info_bed_mentions, add_info, ignore.case = T)
                          ][, .(overall = 1 - nrow(.SD[call_outcome == "Patient declined by facility", ]) /
                                   nrow(.SD[call_outcome %in% c("Patient declined by facility", "Patient accepted by facility")]))])

# Change as Necessary
fac_accept_probs <-
  copy(ed_transfers)[,probability_facility := str_fuzzy_match(facility_contacted)
                     ][!is.na(probability_facility) & !grepl(add_info_bed_mentions, add_info, ignore.case = T),
    .(Rejected = grepl("Patient declined by facility",call_outcome) & !grepl("Patient accepted by facility",call_outcome)),
    by = list(probability_facility, patient_number)
  ][, .(Acceptance_Prob = (1 - mean(as.numeric(Rejected))),N = .N), by = list(probability_facility)]

# Time in between Transfer Requests ---------------------------------------
buffers <- unname(unlist(lapply(split(ed_transfers,by = 'patient_number'), function(df) df[order(event_ts),.(values = diff(event_ts,units = 'hours'))]))/3600)
buffers_params = dist_fit(buffers)

# Internal Admit Review Time ----------------------------------------------
self_coord_mean = one.boot(ed_bh_admits[, disp_to_assign], FUN = mean, R = 1000)

self_coord_estimates = dist_fit(data = c(remove_zeros(data = na.omit(
  object = admit_beds[, bed_assign_timespan]
))))

# Calculating facility coordination time means and distributions (individual hospitals)----------

coord_times <-
  ed_transfers[, .SD[.N > 1], by = list(actual_contact, patient_number)][!grepl(add_info_bed_mentions,add_info), .(review_time = abs(difftime(
    min(event_ts),
    `if`(
      any(grepl(pattern = 'Patient accepted by facility|Patient declined by facility', call_outcome)),
      .SD[call_outcome == 'Patient accepted by facility' | call_outcome == 'Patient declined by facility' , event_ts],
      max(event_ts)
    ),
    units = 'hours'
  ))), by = list(actual_contact, patient_number)][review_time > 0]

fac_coord_occurences <- ed_transfers[, .SD[.N > 1], by = list(actual_contact, patient_number)
             ][!grepl(add_info_bed_mentions,add_info),.(duration = as.numeric(difftime(event_ts,data.table::shift(event_ts,type = 'lag'), unit = 'hours'))),by = list(actual_contact,patient_number)
               ][,.(call_inters = mean(na.omit(duration))),by=actual_contact][ed_transfers[, .SD[.N > 1], by = list(actual_contact, patient_number)
               ][!grepl(add_info_bed_mentions,add_info),][,.N,by = list(patient_number,actual_contact)][,.(avg_contacts = mean(N)-1), by = actual_contact],
               avg_contacts := avg_contacts, on = .(actual_contact)]

fac_coord_times <-
  coord_times[, .(Avg = one.boot(data = review_time,
                                 FUN = mean,
                                 R = 500,
                                 na.rm = T)$t0,
                  Dist = dist_fit(review_time),
                  Count = .N),
              by = .(actual_contact)]

overall_avg = coord_times[, one.boot(review_time, mean, R = 1000,na.rm = T)$t0]
overall_dist = dist_fit(as.numeric(coord_times[, review_time]))

# Calculating average post coordination time -----------------
post_coordination_time <-
  lapply(
    X = c('Internal', 'Transfer'),
    FUN = function(patient_type)
      fitdist(as.numeric(disp_to_dep_data[type == patient_type, post_coordination_time]), distr = 'exp')
  )
names(post_coordination_time) <- c('Internal', 'Transfer')

# Build the siteInfo dataframe and comnbine with previously calculated values ----------------
source(file = file.path("Code",
                        "read_and_format",
                        "Build Siteinfo.R"))
siteInfo <- siteInfo[hospital_systems,
                     hospital_system := hospital_system,
                     on = c(Facility_name = 'name')
                     ][is.na(hospital_system),hospital_system := Facility_name]

siteInfo <-
  siteInfo[,lowered_name := tolower(Facility_name)
           ][fac_coord_times, `:=`(Review_Info = as.numeric(Avg)), on = c("hospital_system == actual_contact")
             ][fac_coord_times, `:=`(Review_Params = Dist), on = c("hospital_system == actual_contact")
               ][ fac_accept_probs, Acceptance_Prob := Acceptance_Prob, on = c('Facility_name'='probability_facility')
                   ][,lowered_name := NULL
                     ][is.na(Acceptance_Prob) | Acceptance_Prob == 0 |Acceptance_Prob == 1,
                       Acceptance_Prob := overall_acceptance_prob]
siteInfo <- siteInfo[which(lapply(siteInfo[,Review_Params],length)==0), `:=`(Review_Params = overall_dist,
                                               Review_Info = overall_avg)]


# Barriers to Placement Prevalence --------------------

n_gram_limit <- 5
barrier_grams <-
  rbindlist(data.table(anti_join(x = orig_generose_patients[, .(word = as.character(tstrsplit(
    gsub(
      pattern = '\\(\\([^()]*\\)\\)|\\([^()]*\\)',
      replacement = '',
      x = BarriersToPlacement
    ),
    split = ',(?![^()]*\\))',
    fill = NA,
    perl = T
  ))), by = 'No'][,word := gsub('de tox|dx|de-tox', replacement = 'detox',word)][
                  ,word := gsub('hx','history', word)][grepl('Yes|No', x = word, ignore.case = T), word := NA_character_, by = No],
  y = stop_words))[!is.na(word),] %>% {function(data) lapply(seq(2:(n_gram_limit+1)), count_n_grams, dt = data, text_col = 'word')}(), fill = T) %>%
  dplyr::select(c(paste0('word_', seq(n_gram_limit)), 'n')) %>%
  filter(n > 10) %>%
  filter(!is.na(word_2)) %>%
  mutate(rate = n/max(orig_generose_patients$No)) %>%
  arrange(desc(n))

barrier_grams <- barrier_grams[,.(barrier= do.call(paste,c(.SD[,paste0('word_',seq(5))],sep = ' ')),rate = rate)][,barrier := gsub(' NA',"",barrier)]
barrier_rates <-
  barrier_grams[barrier %in% c(
    'substance abuse',
    'behavioral dyscontrol',
    'requires video monitoring',
    'developmentally delayed',
    'mn commitment act',
    'medically complex',
    'requires detox',
    'legal history'
  )]

# Assign all the relevant data.frames/inputs/etc. to a input list that is read by the simulation --------------------
hccis =
  data.table(readRDS(file = file.path("src/simulations", "function_requirements", "hccis.rds")))

distance.matrix =
  readRDS(file = file.path("src/simulations",
                           "function_requirements",
                           "Drive Distance Matrix.rds"))
time.matrix =
  readRDS(file = file.path("src/simulations",
                           "function_requirements",
                           "Drive Time Matrix.rds"))

sim_inputs =
  list(
    "ip_arrival_rates" = ip_arrival_rates,
    "ed_arrival_rates" = ed_arrival_rates,
    "siteInfo" = siteInfo,
    "age_frequency" = age_frequency,
    "hccis" = hccis,
    "empirical_dist" = empirical_dist,
    "post_coordination_time" = post_coordination_time,
    "self_coord_estimates" = self_coord_estimates,
    "distance.matrix" = distance.matrix,
    "barrier_rates" = barrier_rates,
    "time.matrix" = time.matrix
    )
saveRDS(
  object = sim_inputs,
  file = file.path(
    "src/simulations",
    "function_requirements",
    "MH_Network_sim_input_list.rds"
  )
)
saveRDS(
  object = siteInfo,
  file = file.path(
    "src/simulations",
    "function_requirements",
    "ip_facilities_info.rds"
))

write.csv(x = copy(siteInfo)[,Review_Params := NULL],
          file = file.path(
            "src/simulations",
            "function_requirements",
            "ip_facilities_info.csv"))

