# Test for whether the non-ED inpatient arrival rate and ED Mmental health patient arrival rate follow non-stationary poission distributions/


# Call script to read in data  files and format
source(
  file = file.path('Code','read_and_format','mayo_data.R')
)

# Call script to read in data  files and format\
source(
  file = file.path('Code','read_and_format','hccis_data.R')
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
