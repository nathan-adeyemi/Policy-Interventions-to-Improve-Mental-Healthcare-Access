# Read other files and functions in
# If running the validation script as a Slurm job, run the input paramter estimation script initially

get_sys_env_var <- function(var_name, var_class){
  res <- Sys.getenv(var_name)
  if(nchar(res) == 0){
    return(NULL)
  } else if (var_class == 'numeric'){
    return(as.numeric(res))
  } else if(grepl('datatable|dataframe',var_class)){
    return(data.table(fromJSON(res)))
  }
}

port_val <-get_sys_env_var(var_name = 'port')
acceptance_probs <-get_sys_env_var(var_name = 'acceptance_probs')

client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)

numIters <- 30
warm_period <-50
sim_period <- 365

tryCatch(expr = {
results <- MH.Network.sim(rep = numIters,
                          warm = warm_period,
                          sim_days = sim_period,
                          acceptance_prob_input = acceptance_probs,
                          resources = TRUE)

},error = function(e){
  cat('An error has occured')
  print(e)
})


origin_date <- as.POSIXct("2019-1-01 00:00:00", tz = "UTC") - (3600 * 24 * warm_period)
hccis <- readRDS(file.path("simulations", "function_requirements", "hccis.rds"))
hccis_melt <- melt(copy(hccis)[,rpt_year := NULL], id.vars = c('hccis_id','owner_hccis_id','urban'), variable.name = "grouping",value.name = 'value')[,grouping := tolower(grouping)]
hccis_melt <- rbind(hccis_melt,hccis_melt[,.(value = sum(value)),by = grouping][grepl('admissions',grouping) & !grepl('icu|ed_to_ip',grouping)][,hccis_id:= 'All'],fill = TRUE)

res_df <-
  results$resources[,day_num := as.numeric(as.factor(lubridate::date(as.datetime(time * 3600, origin_date))))
                  ][cap_change > 0 & day_num > warm_period,
                  ][siteInfo, facility := Facility_name, on = c('resource' = 'Bed_Group')
                  ][, is_Adult := Age %in% c('Adult', 'Geriatric')
                  ][, .(admissions = length(unique(patient))), by = list(replication, facility, is_Adult)
                  ][is.na(admissions),admissions := 0
                  ][, `:=`(grouping = as.character(ifelse(is_Adult, "adult_admissions", "pediatric_admissions")),is_Adult = NULL)
                  ][,.(admissions = one.boot(data = admissions,FUN = mean,R = 500, na.rm = T)$t0, sim_CI = ci_as_text(admissions)), by = list(facility,grouping)
                  ][hccis_melt,`:=`(hccis_admissions = value*(sim_period/365)),on = c("grouping" = "grouping", "facility" = "hccis_id")
                  ][,error := {Vectorize(validate_fun)}(text = sim_CI, true_val = hccis_admissions,differences = T)
                  ][,.SD[!duplicated(.SD,by = 'admissions')],by = 'grouping'][order(facility,grouping)]
res_df <- toJSON(res_df, dataframe = 'columns')
write.socket(client_socket, res_df)

