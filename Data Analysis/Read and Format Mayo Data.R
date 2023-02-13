# Read in Custom Functions-------------------------------------------------
source("functions.R", echo = F)

# Read In Original Data ------------------------------------------------------
all_names <- read_excel(path = file.path(".","Data","HCCIS","hosplist.xlsx"), 
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

aliases <-
  rbindlist(l = lapply(
    X = apply(
      X = data.table(read_excel(
        path  = file.path(".", "Data", "HCCIS", "hosplist.xlsx"),
        sheet = "Unique Facility Aliases"
      )),
      MARGIN = 2,
      FUN = na.omit
    ),
    FUN = function(i)
      data.table(name = rep(i[1], length(unique(
        i
      ))), aliases = tolower(unique(i)))
  ))[aliases != '0',]


# health_systems <- apply(
#   X = data.table(read_excel(
#     path  = file.path(".", "Data", "HCCIS", "hosplist.xlsx"),
#     sheet = "Hospital Systems"
#   )),
#   MARGIN = 2,
#   FUN = na.omit
# )
# health_systems <- rbindlist(lapply(
#   X = health_systems,
#   FUN = function(i)
#     data.table(system = rep(i[1], length(unique(i))),
#                alias = i)
# ))

hospital_systems <- data.table(read_excel(
    path  = file.path(".", "Data", "HCCIS", "hosplist (in use).xlsx"),
    sheet = "Hospital List",skip = 5))[,c('Hospital Name','System Affiliation')
                              ][,`:=`(name = `Hospital Name`,hospital_system = `System Affiliation`)
                                ][,`:=`(`Hospital Name` = NULL,
                                        `System Affiliation` = NULL)
                                  ][!is.na(hospital_system) & hospital_system != 'No Affiliation'
                                    ][,name := gsub(pattern = '\\*','',name)]


distance.matrix <-
  readRDS(file = file.path(
    ".",
    "Data",
    "Function Requirements",
    "Drive Distance Matrix.rds"
  ))

transferred_admits <-
  data.table(read_excel(
    path = file.path(".",
                     "Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "Transfer Admits"
  ))

ed_transfers <-
  data.table(read_excel(
    path = file.path(".",
                     "Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "Transfers Out"
  )) %>%
  setorder(event_ts)

ed_bh_admits <-
  data.table(read_excel(
    path = file.path(".",
                     "Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "ED Admits"
  ))

admit_beds <-
  data.table(read_excel(
    path = file.path(".",
                     "Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "Transfer + Other Admit Beds"
  ))

all_beds <-
  data.table(read_excel(
    path = file.path(".",
                     "Data",
                     "Mayo Clinic Data",
                     "Psych Patients Combined.xlsx"),
    sheet = "All Patients Events"
  ))

# Transferred ED -> IP Patient Data set (read data and modify) -------------------

ed_transfers <-
  ed_transfers[adt_discharge_disp %in% c(
    "Acute Care Hospital",
    "Admitted as an Inpatient",
    "Crtical Access Hospital",
    "Psychiatric Hospital",
    "Psychiatric Hospital w/ Planned Readmission"
  ) &
    ed_disp_final %in% c("Transfer to Health Care Facility", "Admit")
  ][,`:=`(lower_contact = tolower(facility_contacted),
          lower_receive = tolower(receiving_facility))
  ][aliases,fac_contact_name := name, on = c(lower_contact = 'aliases')
     ][aliases, rec_fac_name := name, on = c(lower_receive = 'aliases')
        ][,`:=`(facility_contacted = fac_contact_name,
                  receiving_facility = rec_fac_name)
             ][,`:=`(fac_contact_name = NULL,
                     rec_fac_name = NULL)]

ed_transfers[grepl("decline", add_info, ignore.case = T) & call_outcome != "Patient declined by facility", call_outcome := "Patient declined by facility"
             ][, include := sum(is.na(receiving_facility)) != 0 & 
                 !("Patient accepted by facility" %in% call_outcome), by = patient_number
               ]

indices <- ed_transfers[include == TRUE & !is.na(receiving_facility), .I[which.max(event_ts)], by = patient_number]$V1
ed_transfers[indices, call_outcome := "Patient accepted by facility"
             ][call_outcome == "Patient accepted by facility", Accept_Time := event_ts
               ][, `:=`(ts_1 = as.POSIXct(accept_time_func(Accept_Time, event_ts), origin = "1970-01-01")),by = patient_number
                ][, `:=`(
                  Accept_Time = NULL,
                  ed_LoS = ed_LoS / 60,
                  # receiving_facility = sub_alias(receiving_facility),
                  # facility_contacted = sub_alias(facility_contacted),
                  ED_name = {Vectorize(ED.classify)}(ED),
                  include = NULL)
                  ][, `:=`(age_group = {Vectorize(age.classify)}(age),
                           disp_to_dep = as.numeric(abs(difftime(
                             time1 = min(c(event_ts,ed_disp_ts), na.rm = T),
                             time2 = ed_departure,
                             units = "hours"))),
                           disp_to_assign = as.numeric(abs(difftime(
                             time1 = min(c(event_ts,ed_disp_ts), na.rm = T),
                             time2 = max(ts_1, ed_disp_ts, na.rm = T),
                             units = "hours"))),
                           type = "External Transfer"), by = patient_number,
                ][, rejections := .SD[call_outcome == "Patient declined by facility",max(0,.N,na.rm = T)], by = patient_number
                  ][!is.na(transportation_mode), Travel.Distance := {Vectorize(find_drive_distance)}(ED_name, receiving_facility)
                    ][,Travel.Distance := max(Travel.Distance,na.rm = T),by = patient_number
                      ][is.na(facility_contacted) & !is.na(receiving_facility), facility_contacted := receiving_facility
                        ][hospital_systems,actual_contact := hospital_system,on = c(facility_contacted = 'name')
                          ][,`:=`(lower_contact = NULL,
                                  lower_receive = NULL)
                            ][is.na(actual_contact), actual_contact := facility_contacted]

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
    age_group = {Vectorize(age.classify)}(age),
    hospital_discharge_ts = NULL,
    ip_unit = {Vectorize(unit.classify)}(ip_unit),
    ed_day = lubridate::wday(ed_arrival, label = T, abbr = T),
    year = lubridate::year(ip_admission_ts)
  )]

admit_beds <- admit_beds[, destination_department := {Vectorize(unit.classify)}(destination_department)
                         ][, bed_transfer_instance := as.numeric(rownames(.SD)), by = patient_number
                           ][,bed_assign_timespan := as.numeric(abs(difftime(requested_time,last_assigned_time,units = 'hours')))]
# Finds all patients in both ED BH admits and all beds
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
                                        location_description = {Vectorize(unit.classify)}(location_description),
                                        admit_date = NULL,
                                        discharge_date = NULL,
                                        site = NULL,
                                        event_los = event_los / 60,
                                        source = {Vectorize(source.classify)}(admit_source),
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
                                  )
                                ]

# Every Bed BH Admits Occupied (including non Generose) -------------------
all_ip_admits <- rbindlist(list(
  copy(all_ip_changes)[
    event_type != "Discharge",
    list(patient_number, event_type, location_description,
      "ip_admission_ts" = event_date, "ip_discharge_ts" = event_end_date, age, bed,
      "source" = admit_source, bed_request_ts
    )
  ][, dataset := "all_beds"] %>% data.table(),
  copy(ed_bh_admits)[, list(patient_number, ip_unit, ip_admission_ts, ip_discharge_ts, age, bed, bed_request_ts)
                     ][, `:=`(event_type = "Admission", source = "ED", dataset = "ed_bh_admits")] %>%
    relocate("event_type", .after = "patient_number") %>%
    rename(
      location_description = ip_unit,
      ip_admission_ts = ip_admission_ts,
      ip_discharge_ts = ip_discharge_ts
    )
), fill = T)[, `:=`(
  patient_number = as.numeric(as.factor(paste(patient_number, dataset))),
  dataset = NULL, source_general = {Vectorize(source.classify)}(source)
)][order(ip_admission_ts), ][location_description %in% c("Adult Psych", "Geriatric/Med Psych", "Child/Adolescent Psych", "Mood Disorders Unit")][, .SD[1], by = patient_number]

# All Behavioral Admits Data set ------------------------------------------
transferred_admits <- 
   transferred_admits[ip_admit == "Y", 
                         ][,`:=`(unit = {Vectorize(unit.classify)}(ip_unit),
                                 age = NULL)
                           ][all_ip_changes[!is.na(transferred_admits_patient_num)], 
                             `:=`(age = age), 
                             on = c(patient_number = 'transferred_admits_patient_num')
                             ][,age_group := {Vectorize(age.classify)}(age)]

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
  data.table(df = "all_ip_admits", all_ip_admits[, .(
    min_date = as.POSIXct(sapply(.SD, function(x) min(x)), origin = "1970-01-01 00:00:00"),
    max_date = as.POSIXct(sapply(.SD, function(x) max(x)), origin = "1970-01-01 00:00:00")
  ), .SDcols = c("ip_admission_ts")]),
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
  rbindlist(list(ed_bh_admits[, list(age_group = age_group,
                                     ed_arrival = disp_ts,
                                     ESI = ESI,
                                     disp_to_dep = disp_to_dep,
                                     disp_to_assign = disp_to_assign,
                                     type = 'Internal')],
                 unique(ed_transfers[ED == 'RST ROMB ED' &
                 # unique(ed_transfers[ED == 'RST ROMB ED' | receiving_facility == 'Mayo Clinic Hospital - Rochester' &                     
                 # unique(ed_transfers[
                                       ed_disp_final %in% c('Admit', 'Transfer to Health Care Facility') &
                                       gsub(" w/ Planned Readmission", "", adt_discharge_disp) %in%
                                       c(
                                         'Acute Care Hospital',
                                         'Admitted as an Inpatient',
                                         "Another Health Care Institution Not Defined",
                                         "Psychiatric Hospital",
                                         "Still a Patient",
                                         "Critical Access Hospital"
                                       ),
                                     list(
                                       age_group = age_group,
                                       ed_arrival = ed_disp_ts,
                                       ESI = ESI,
                                       disp_to_dep = disp_to_dep,
                                       disp_to_assign = disp_to_assign,
                                       type = "Transfer"
                                     ), by = patient_number][,patient_number := NULL]),
                 all_beds[event_type == 'Admission' & 
                            grepl('ROMB ED', location_description) & 
                            admit_source == "Non-Health Care Facility Point of Origin", list(age_group = {Vectorize(age.classify)}(age), ed_arrival = admit_date)
                 ][ed_arrival < min(c(max(ed_bh_admits[,disp_ts]),
                                      max(ed_transfers[,ed_disp_ts]),
                                      max(all_beds[event_type == 'Admission' & 
                                                     grepl('ROMB ED', location_description) & 
                                                     admit_source == "Non-Health Care Facility Point of Origin",admit_date])))
                   ][, `:=`(
                     ESI = NA_integer_,
                     disp_to_dep = NA_real_,
                     disp_to_assign = NA_real_,
                     type = 'Internal Admit (all beds set)')]), use.names = T, fill = T)[, `:=`(
                       year = lubridate::year(ed_arrival),
                       day = lubridate::wday(ed_arrival, label = T, abbr = F)
                     )][, dates := as.Date(ed_arrival)
                        ][date_list, prog_day_I := prog_day, on = .(dates)
                          ][, `:=`(dates = NULL,
                                   keep = NULL)][!is.na(ed_arrival), ]

# Individual Unit Arrival Rates -----------------------------
max_unit_beds = c('Child/Adolescent Psych' = 18, 'Adult Psych' = 41, 'Geriatric/Med Psych' = 14)
# max_unit_beds = c('Child/Adolescent Psych' = 18, 'Adult Psych' = 25, 'Geriatric/Med Psych' = 14, 'Mood Disorders Unit' =  16)

unit_dynamics <-
  distinct(rbind(all_ip_changes[location_description != 'ED' &
                                event_end_date < Sys.Date()
                              ][, `:=`(
                                instance = as.numeric(rownames(.SD)),
                                unit_change = rleid(location_description)),
                                by = patient_number][, .(
                                  start = min(event_date),
                                  exit = max(event_end_date),
                                  age = unique(age),
                                  transferred_admits_patient_num = transferred_admits_patient_num
                                ), by = list(patient_number,
                                  event_type,
                                  unit_change,
                                  location_description)
                                ][event_type != 'Discharge'
                                  ][, `:=`(
                                    type = 'Non-ED Admission',
                                    unit_change = NULL)],
               ed_bh_admits[, list(patient_number,
                                   ed_disp_init,
                                   ip_unit,
                                   ip_admission_ts,
                                   ip_discharge_ts,
                                   age)][, `:=`(transferred_admits_patient_num = NA,
                                                type = 'ED Admission')], 
               use.names = F)[, idx := as.numeric(as.factor(paste(patient_number, type, sep = '_')))
                              ][, `:=`(patient_number = NULL)
                                ][order(start, decreasing = F)
                                  ][, `:=`(ed_patient = type == 'ED Admission')
                                    ][, dates := as.Date(start)
                                      ][date_list, prog_day_I := prog_day, on = .(dates)
                                        ][, dates := NULL
                                          ][, `:=`(transfer = !is.na(transferred_admits_patient_num))
                                            ][transfer == TRUE, type := 'ED Admission'
                                              ][type == 'ED Admission', ed_patient := T
                                                ][location_description == 'Mood Disorders Unit',
                                                  location_description := 'Adult Psych'
                                                  ][,`:=`(start = min(start),
                                                          exit = max(exit),
                                                          location_description = unique(location_description)[1]),
                                                    by = idx],
           location_description,start,exit,idx,.keep_all = T)

# ed_transfers <- ed_transfers[event_ts >= date_range[, min_date] & event_ts <= date_range[, max_date], 
#                              ][grepl('Unspecified',ESI,ignore.case = T),ESI := NA_integer_
#                                ][,ESI := as.numeric(ESI)
#                                  ][,dates := as.Date(ed_arrival)
#                                    ][date_list, prog_day_I := prog_day, on = .(dates)
#                                      ][,dates := NULL]

transferred_admits <- transferred_admits[event_ts >= date_range[, min_date] & event_ts <= date_range[, max_date], 
                                         ][,dates := as.Date(ip_start)
                                           ][date_list, prog_day_I := prog_day, on = .(dates)
                                             ][,dates := NULL]

ed_bh_admits <- ed_bh_admits[ed_arrival >= date_range[, min_date] & ip_discharge_ts <= date_range[, max_date], 
                             ][,dates := as.Date(ip_admission_ts)
                             ][date_list, prog_day_I := prog_day, on = .(dates)
                             ][,dates := NULL]

all_ip_admits <- all_ip_admits[, keep := (ip_admission_ts >= date_range[, min_date] & ip_discharge_ts < date_range[, max_date])
                               ][keep == TRUE, 
                                 ][,dates := as.Date(ip_admission_ts)
                                   ][date_list, prog_day_I := prog_day, on = .(dates)
                                   ][,`:=`(dates = NULL, 
                                           keep = NULL)]

all_ip_changes <- all_ip_changes[event_date >= date_range[, min_date] & event_date < date_range[, max_date], 
                                 ][,dates := as.Date(event_date)
                                 ][date_list, prog_day_I := prog_day, on = .(dates)
                                 ][,`:=`(dates = NULL)]

unit_dynamics <- unit_dynamics[exit <= as.POSIXct("2022-01-01")][start >= as.Date(unit_dynamics[transfer == T][1,start])]



# Code for creating Longitudnal version of patient entering the IP units -------------------
# patient_longitudinal <- all_ip_admits[,variable := as.numeric(row.names(.SD)),by = patient_number]
# max_events <- max(patient_longitudinal$variable)
# max_unit_beds = c('Child/Adolescent Psych' = 18, 'Adult Psych' = 25, 'Geriatric/Med Psych' = 14, 'Mood Disorders Unit' = 16)
# patient_longitudinal <- data.table(dcast(patient_longitudinal,
#                               age + patient_number +
#                                 admit_mode + admit_source +
#                                 discharge_disposition ~ variable,
#                               value.var = c('event_type','event_date','event_los','location_description','patient_process_type')))[order(event_date_1)]
# patient_longitudinal <- setcolorder(patient_longitudinal,c(setdiff(colnames(patient_longitudinal),
#                                                        grep('event_type|event_date|event_los|location_description|patient_process_type',colnames(patient_longitudinal),value=T)),
#                                                data.table(reshape2::melt(sapply(c('event_type','event_date','event_los','location_description','patient_process_type'),paste0,'_',seq(max_events))))[order(Var1),value,with = T]))
#
# emergency_patients <- patient_longitudinal[patient_longitudinal[,Reduce(`|`, lapply(.SD, `==`, 'Emergency')),
#                                                                 .SDcols = grep('patient_process_type',names(emergency_patients),value=T)]]
