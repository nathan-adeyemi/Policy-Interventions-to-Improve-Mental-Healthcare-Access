str_fuzzy_match <-
  function(string,
           df = aliases,
           names_col = 'name',
           alias_col = 'aliases') {
    return(unlist(df[, ..names_col])[unlist(lapply(lapply(
      map2(.x = string, .y = df[, ..alias_col], .f = stringdist), which.min
    ), function(i)
      return(ifelse(
        is_empty(i), NA_integer_, i
      ))))])
  }


# Read In Original Data ------------------------------------------------------
all_names <- readxl::read_excel(path = file.path("Data","HCCIS","hosplist.xlsx"),
                        sheet = "Unique Facility Aliases") %>%
  apply(2, FUN = function(x) {
    as.character(na.omit(x)) %>%
      {
        function(i) i[i != "0"]
      }() %>%
      unique() %>%
      tolower()
  }) %>%
  names()

aliases <-data.table(readxl::read_excel(
  path  = file.path("Data", "HCCIS", "hosplist.xlsx"),
  sheet = "Unique Facility Aliases"
))
aliases <- rbindlist(lapply(X = colnames(aliases),
                            FUN = function(col){
                       unique_names <- unlist(na.omit(unique(aliases[,..col])))
                       return(data.table(name = rep(col, length(unique_names)), aliases = unique_names))
                     }), 
                     fill = TRUE)

hospital_systems <- data.table(readxl::read_excel(
    path  = file.path("Data", "HCCIS", "hosplist.xlsx"),
    sheet = "Hospital List",skip = 5))[,c('Hospital Name','System Affiliation')
                              ][,`:=`(name = `Hospital Name`,hospital_system = `System Affiliation`)
                                ][,`:=`(`Hospital Name` = NULL,
                                        `System Affiliation` = NULL)
                                  ][!is.na(hospital_system) & hospital_system != 'No Affiliation'
                                    ][,name := gsub(pattern = '\\*','',name)]

distance.matrix <-
  readRDS(file = file.path(
    "simulations",
    "function_requirements",
    "Drive Distance Matrix.rds"
  ))

transferred_admits <-
  data.table(readxl::read_excel(
    path = file.path("Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "Transfer Admits"
  ))

ed_transfers <-
  data.table(readxl::read_excel(
    path = file.path("Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "Transfers Out"
  )) %>%
  setorder(event_ts)

ed_bh_admits <-
  data.table(readxl::read_excel(
    path = file.path(
                     "Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"
                     ),
    sheet = "ED Admits"
  ))

admit_beds <-
  data.table(readxl::read_excel(
    path = file.path(
                     "Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "Transfer + Other Admit Beds"
  ))

all_beds <-
  data.table(readxl::read_excel(
    path = file.path(
                     "Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "All Patients Events"
  ))

orig_generose_patients <- data.table(readxl::read_excel(
  path = file.path('Data',
                   'Mayo Clinic Data',
                   'Psych Patients Combined.xlsx'),
  sheet = 'Nasibeh ED Arrivals'
))

# Transferred ED -> IP Patient Data set (read data and modify) -------------------
ed_transfer_list <- vector('list',ed_transfers[,.N])
for( i in seq(ed_transfers[,.N])){
  ed_transfer_list[[i]] <- fix_and_expand_fac_contacted(ed_transfers[i,])
}
ed_transfers <- rbindlist(ed_transfer_list)

ed_transfers <-
  ed_transfers[adt_discharge_disp %in% c(
    "Acute Care Hospital",
    "Admitted as an Inpatient",
    "Crtical Access Hospital",
    "Psychiatric Hospital",
    "Psychiatric Hospital w/ Planned Readmission"
  ) &
    ed_disp_final %in% c("Transfer to Health Care Facility", "Admit")
  ][,`:=`(facility_contacted = tolower(facility_contacted),
          receiving_facility = tolower(receiving_facility))
    # Merging with hospital alias and hospital system dataframes
    # ][aliases,facility_contacted := name, on = c(facility_contacted = 'aliases')
    #   ][aliases, receiving_facility := name, on = c(receiving_facility = 'aliases')
  ][,corrected_name := lapply(.SD,str_fuzzy_match), .SDcols = 'facility_contacted'
    ][is.na(corrected_name),corrected_name := lapply(.SD,str_fuzzy_match), .SDcols = 'receiving_facility'
        ][hospital_systems,actual_contact := hospital_system,on = c(corrected_name = 'name')
        ][is.na(actual_contact) & !is.na(corrected_name), actual_contact := corrected_name
        ][grepl("decline|no|unable|on divert|acuity", add_info, ignore.case = T) & call_outcome != "Patient declined by facility", call_outcome := "Patient declined by facility"]           # Properly assigning additional rejection call outcomes to the 

ed_transfers <- ed_transfers[, rejections := .SD[call_outcome == "Patient declined by facility",max(0,.N,na.rm = T)], by = patient_number
                  ][is.na(actual_contact), actual_contact := facility_contacted
                    ][call_outcome == 'Patient accepted by facility', ip_treatment_facility := receiving_facility,
                    ][,`:=`(ED_name = {Vectorize(ED_classify)}(ED),
                            corrected_name = NULL)
                     ][,`:=`(Travel.Distance = {Vectorize(find_drive_distance)}(ED_name, ip_treatment_facility),
                             age_group = age_classify(age),
                             ed_LoS = ed_LoS/60)
                      ][,`:=`(Travel.Distance = max(Travel.Distance,na.rm = T),
                              first_event_ts = min(event_ts),
                              final_event_ts = max(event_ts)),
                        by=patient_number]

multiple_rows = unlist(ed_transfers[,.N,by=patient_number][N>1,patient_number])
accepted_patients <- unlist(ed_transfers[call_outcome == 'Patient accepted by facility',patient_number])
boarding_patients <- intersect(multiple_rows,accepted_patients)

wrong_disp_ts_patients <- copy(ed_transfers[patient_number %in% boarding_patients,])[,`:=`(diff1 = abs(difftime(first_event_ts,ed_disp_ts,units = 'hours')), diff2 = abs(difftime(final_event_ts,ed_disp_ts,units = 'hours'))),by=patient_number][diff2 < diff1][,patient_number]
ed_transfers <- ed_transfers[patient_number %in% wrong_disp_ts_patients, ed_disp_ts := min(event_ts),by = patient_number]

transfer_boarding <-
  ed_transfers[order(patient_number, event_ts),
               ][patient_number %in% boarding_patients,
                 `:=`(accept_time = min(.SD[grepl('accept',call_outcome),event_ts])),by = patient_number
                 ][patient_number %in% boarding_patients,
                  .(
                    ED = unique(ED),
                    age_group = unique(age_group),
                    type = 'Transfer',
                    ed_disp_ts = unique(ed_disp_ts),
                    ESI = unique(ESI),
                    vulnerable = unique(age_group != 'Adult'),
                    ed_LoS = unique(ed_LoS),
                    med_clearance = unique(as.numeric(abs(difftime(time1 = ed_arrival,
                                                            time2 = min(ed_disp_ts,.SD[which.min(seq_along(patient_number)),event_ts]))))),
                    post_coordination_time = .SD[which(grepl('accept', call_outcome)),
                                                 as.numeric(abs(difftime(
                                                   time1 = event_ts,
                                                   time2 = ed_departure,
                                                   unit = 'hours'
                                                 )))],
                    boarding_time = unique(
                      ed_LoS - difftime(
                        time1 = ed_disp_ts,
                        time2 = ed_arrival,
                        unit = 'hours'
                      )
                    ),
                    coordination_time =
                      max(.SD[call_outcome == 'Patient accepted by facility', as.numeric(abs(difftime(
                        time1 = event_ts,
                        time2 = ed_disp_ts,
                        unit = 'hours'
                      )))],
                      .SD[1,
                          as.numeric(abs(difftime(time1 = accept_time,
                                   time2 = first_event_ts,
                                   unit = 'hours')))])
                  ), by = patient_number]

# Internal ED -> IP Patients Dataset (read data and calculate time --------
ed_bh_admits <-
  ed_bh_admits[, disp_to_dep := (abs(as.numeric(
    difftime(
      time1 = min(disp_ts, bed_request_ts),
      time2 = ed_departure,
      units = "hours"
    )
  ))), by = patient_number][, disp_to_assign := abs(as.numeric(difftime(
    time1 = min(disp_ts, bed_request_ts),
    time2 = bed_assigned_ts,
    units = "hours"
  ))), by = patient_number][, `:=`(
    type = "Internal",
    age_group = {
      Vectorize(age_classify)
    }(age),
    hospital_discharge_ts = NULL,
    ip_unit = unit_classify(ip_unit,age),
    ip_unit_full = unit_classify(ip_unit,age,sim_unit_name = T),
    ed_day = lubridate::wday(ed_arrival, label = T, abbr = T),
    year = lubridate::year(ip_admission_ts)
  )][, `:=`(post_coordination_time = as.numeric(max(abs(
    difftime(
      time1 = bed_assigned_ts,
      time2 = bed_request_ts,
      units = 'hours'
    )
  ),
  abs(
    difftime(
      time1 = ip_admission_ts,
      time2 = bed_assigned_ts,
      units = 'hours'
    )
  ),
  abs(
    difftime(
      time1 = ip_admission_ts,
      time2 = bed_request_ts,
      units = 'hours'
    )
  )))), by = patient_number
  ][, ed_LoS := ed_LoS/60]

admit_beds <- admit_beds[, bed_transfer_instance := as.numeric(rownames(.SD)), by = patient_number
                           ][,`:=`(bed_assign_timespan = as.numeric(abs(difftime(requested_time,last_assigned_time,units = 'hours'))),
                                   destination_department = unit_classify(destination_department))]
# Finds and remove all patients in both ED BH admits and all beds
all_beds <- all_beds[!(patient_number %in% na.omit(copy(all_beds
                                                        )[, ed := "Emergency" %in% patient_process_type, by = patient_number
                                                          ][ed == TRUE,
                                                            ][, .SD[1, ], by = patient_number
                                                              ][ed_bh_admits,
                                                                roll = "nearest",
                                                                on = c(age = "age",
                                                                       discharge_date = "ip_discharge_ts",
                                                                       admit_date = "ed_arrival")
                                                                ][, patient_number]))]

# Non-ED IP Admissions Transfers and Discharges ----------------------------
all_ip_changes <- copy(all_beds)[, `:=`(bed = {Vectorize(extract_bed)}(location_description),
                                        location_description = unit_classify(location_description,age),
                                        location_description_full = unit_classify(location_description,age,sim_unit_name = T),
                                        admit_date = NULL,
                                        discharge_date = NULL,
                                        site = NULL,
                                        event_los = event_los / 60,
                                        source = {Vectorize(source_classify)}(admit_source),
                                        age = age)
                                 ][location_description %in% c("Adult Psych", "Geriatric/Med Psych", "Child/Adolescent Psych", "Mood Disorders Unit") &
                                     event_type != "Discharge",by = patient_number
                                ][event_type == "Transfer In", event_type := "Transfer"
                                  ][admit_beds, `:=`(bed_request_ts = requested_time,
                                                     transfer_in = in_ip_admits,
                                                     transferred_admits_patient_num = ip_admits_patient_number),
                                  on = c(
                                    patient_number = "patient_number",
                                    location_description = "destination_department",
                                    event_type = "event_type",
                                    bed = "destination_bed"
                                  )]

# Number of treatment beds @ each Mayo Clinic Unit -----------------------------
# Current split (All MDU and hald of the Geriatric/Med Psych beds are for 18-64 patients)
max_unit_beds = c('Child/Adolescent Psych' = 18, 'Adult Psych' = 48, 'Geriatric/Med Psych' = 7)

# Old split assuming Geriatric beds are for 65+ ONLY
# max_unit_beds = c('Child/Adolescent Psych' = 18, 'Adult Psych' = 41, 'Geriatric/Med Psych' = 14)

# Old split considering the Mood Disorders Unit as a completely separate unit
# max_unit_beds = c('Child/Adolescent Psych' = 18, 'Adult Psych' = 25, 'Geriatric/Med Psych' = 14, 'Mood Disorders Unit' =  16)

# All Behavioral Admits Data set ------------------------------------------
transferred_admits <-
   transferred_admits[ip_admit == "Y",
                         ][,`:=`(unit =  unit_classify(ip_unit,age),
                                 age_group = age_classify(age),
                                 age = NULL)]

transferred_admits$ip_start <-
   merge(x = transferred_admits,
         y = copy(admit_beds
                  )[!is.na(ip_admits_patient_number)
                    ][all_ip_changes,
                      ip_start := event_date,
                      on = c(patient_number = 'patient_number',
                             event_type = 'event_type')
                      ][,.SD[1,],by = patient_number
                        ][!is.na(ip_start)],
         by.x = 'patient_number',
         by.y = 'ip_admits_patient_number',
         all.x = T)$ip_start

date_range <- rbind(
  data.table(df = "ed_transfers", ed_transfers[, .(min_date = min(event_ts), max_date = max(event_ts))]),
  data.table(df = "transferred_admits", transferred_admits[, .(min_date = min(event_ts), max_date = max(event_ts))]),
  data.table(df = "ed_bh_admits", ed_bh_admits[, .(min_date = min(ed_arrival), max_date = max(ip_discharge_ts, na.rm = T))]),
  data.table(df = "all_ip_changes", all_ip_changes[, .(min_date = min(event_date), max_date = max(event_date))])
)[, .(
  min_date = max(min_date),
  max_date = min(max_date))]

date_list <- data.table(dates = seq(from = as.Date(date_range[,min_date]),
                                    to = as.Date(date_range[,max_date]),
                                    by = 'days')
                        )[,prog_day := as.numeric(as.factor(dates))]
# Data for calculation the disposition to admit decison and bed assignment timespans ----------------------
disp_to_dep_data <-
  rbindlist(list(ed_bh_admits[, list(
    ED = 'RST ROMB ED',
    age_group = age_group,
    ed_arrival = disp_ts,
    ed_LoS = ed_LoS,
    boarding_hours = boarding_hours,
    type = 'Internal',
    medical_clearance_hours = as.numeric(difftime(
      time1 = disp_ts,
      time2 = ed_arrival,
      unit = 'hours'
    )),
    post_coordination_time = post_coordination_time,
    ESI = ESI
  )],
  unique(transfer_boarding[,
                           .(
                             ED = ED,
                             ESI = ESI,
                             age_group = age_group,
                             type = 'Transfer',
                             ed_LoS = ed_LoS,
                             ed_arrival = ed_disp_ts,
                             medical_clearance_hours = med_clearance,
                             boarding_hours = boarding_time,
                             coordination_time = coordination_time,
                             post_coordination_time = post_coordination_time
                           )]),
  all_beds[event_type == 'Admission' &
             grepl('ROMB ED', location_description) &
             admit_source == "Non-Health Care Facility Point of Origin", list(age_group = {
               Vectorize(age_classify)
             }(age), ed_arrival = admit_date)][ed_arrival < min(c(max(ed_bh_admits[, disp_ts]),
                                                                  max(ed_transfers[, ed_disp_ts]),
                                                                  max(all_beds[event_type == 'Admission' &
                                                                                 grepl('ROMB ED', location_description) &
                                                                                 admit_source == "Non-Health Care Facility Point of Origin", admit_date])))][, `:=`(
                                                                                   ESI = NA_integer_,
                                                                                   boarding_hours = NA_real_,
                                                                                   medical_clearance_hours = NA_real_,
                                                                                   type = 'Internal Admit (all beds set)'
                                                                                 )]),
  use.names = T,
  fill = T)[, `:=`(
    year = lubridate::year(ed_arrival),
    day = lubridate::wday(ed_arrival, label = T, abbr = F)
  )][, dates := as.Date(ed_arrival)][date_list, prog_day_I := prog_day, on = .(dates)][, `:=`(dates = NULL)][!is.na(ed_arrival),]

unit_dynamics <-
  distinct(rbind(all_ip_changes[location_description != 'ED' &
                                  event_end_date < Sys.Date() &
                                  patient_process_type == 'IP Behavioral Health'
  ][, `:=`(
    instance = as.numeric(rownames(.SD)),
    unit_change = rleid(location_description)),
    by = patient_number][, .(
      start = min(event_date),
      exit = max(event_end_date),
      age = unique(age),
      transferred_admits_patient_num = transferred_admits_patient_num,
      location_description_full = location_description_full 
    ), by = list(patient_number,
                 event_type,
                 unit_change,
                 location_description)
    ][event_type != 'Discharge'
    ][, `:=`(
      type = ifelse(is.na(transferred_admits_patient_num),'Non-ED Admission','ED Admission'),
      unit_change = NULL,
      dataset = 'all_ip_changes')],
  ed_bh_admits[, list(patient_number,
                      location_description = ip_unit,
                      location_description_full = ip_unit_full,
                      start = ip_admission_ts,
                      exit = ip_discharge_ts,
                      age)][,`:=`(
                        dataset = 'ed_bh_admits')
                      ][, `:=`(transferred_admits_patient_num = NA,type = 'ED Admission')],
  use.names = T, 
  fill = TRUE)[, idx := as.numeric(as.factor(paste(patient_number, dataset, sep = '_')))
               # ][, `:=`(patient_number = NULL)
  ][order(start, decreasing = F)
  ][, `:=`(ed_patient = type == 'ED Admission',
           transfer = !is.na(transferred_admits_patient_num))
  ][, dates := as.Date(start)
  ][date_list, prog_day_I := prog_day, on = .(dates)
  ][, dates := NULL
  ][transfer == TRUE, type := 'ED Admission'
  ][location_description == 'Mood Disorders Unit',`:=`(location_description = 'Adult Psych', location_description_full = 'Mayo Rochester Adult ')
  ][,`:=`(start = min(start),
          exit = max(exit)),by = idx,
  ][length(list(unique(location_description))) == 2 & any(grepl('Geriatric', location_description)),
    `:=`(location_description = 'Geriatric/Med Psych',location_description_full = 'Mayo Rochester Geriatric')],
  location_description,start,exit,idx,.keep_all = T)


transferred_admits <- transferred_admits[event_ts >= date_range[, min_date] & event_ts <= date_range[, max_date],
                                         ][,dates := as.Date(ip_start)
                                           ][date_list, prog_day_I := prog_day, on = .(dates)
                                             ][,dates := NULL]

ed_bh_admits <- ed_bh_admits[ed_arrival >= date_range[, min_date] & ip_discharge_ts <= date_range[, max_date],
                             ][,dates := as.Date(ip_admission_ts)
                             ][date_list, prog_day_I := prog_day, on = .(dates)
                             ][,dates := NULL]


all_ip_changes <- all_ip_changes[event_date >= date_range[, min_date] & event_date < date_range[, max_date],
                                 ][,dates := as.Date(event_date)
                                 ][date_list, prog_day_I := prog_day, on = .(dates)
                                 ][,`:=`(dates = NULL)]

unit_dynamics <- unit_dynamics[exit <= as.POSIXct("2022-01-01")][start >= as.Date(unit_dynamics[unit_dynamics$transfer == T][1,start])]
