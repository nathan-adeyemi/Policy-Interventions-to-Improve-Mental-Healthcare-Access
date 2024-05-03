
get_sys_env_var <- function(var_name, var_class = 'numeric') {

  # Function that grabs relecant environment variables from the encompassing shell session.
  # Environment variables are added by the parent python process 

  res <- Sys.getenv(var_name)
  if (nchar(res) == 0) {
    return(NULL)
  } else if (var_class == 'numeric') {
    return(as.numeric(res))
  } else if (grepl('datatable|dataframe', var_class)) {
    print(res)
    return(data.table(fromJSON(res)))
  }
}

read_json_con <- function(socket, max_length = 256){
  json_string <- ""
  
  while(TRUE){
    new_piece <- read.socket(socket)
    json_string <- paste0(json_string, new_piece)
    if(nchar(new_piece) < max_length){
      break
    }
  }
  return(json_string)
}

gen_hyperparam_res <- function(resource_df,receiver,warmup){

  # Function that return the simulation results to return to the search algorithm
  res_df <-  copy(resource_df)[, day_num := as.numeric(as.factor(lubridate::date(as.datetime(
    time * 3600, origin_date
  ))))][cap_change > 0 &
          is.na(patient) &
          day_num > warmup, patient := generate_random_id(13)][cap_change > 0 &
                                                                day_num > warmup,][siteInfo, facility := Facility_name, on = c('resource' = 'Bed_Group')][, is_Adult := Age %in% c('Adult', 'Geriatric')][, .(admissions = length(unique(patient))), by = list(replication, facility, is_Adult)][is.na(admissions), admissions := 0][, .(facility = 'All', admissions = sum(admissions)), by = list(is_Adult, replication)][, `:=`(grouping = as.character(ifelse(
                                                                  is_Adult, "adult_admissions", "pediatric_admissions"
                                                                )), is_Adult = NULL)]
  res_df <-
    rbind(
      res_df,
      melt(
        res_df[, .(total_admissions = sum(admissions)), by = list(replication, facility)],
        id.vars = c('replication', 'facility'),
        variable.name = 'grouping',
        value.name = 'admissions'
      )
    )[, .(admissions = one.boot(data = admissions,FUN = mean, R = 500, na.rm = T)$t0,
          sim_CI = ci_as_text(admissions)), by = list(facility, grouping)][hccis_melt, `:=`(hccis_admissions = value *
                                                                                              (sim_period / 365)), on = c("grouping" = "grouping", "facility" = "hccis_id")][, error := {
                                                                                                Vectorize(validate_fun)
                                                                                              }(text = sim_CI,
                                                                                                true_val = hccis_admissions,
                                                                                                differences = T)][, .SD[!duplicated(.SD, by = 'admissions')], by = 'grouping'][order(facility, grouping)]

  print('Results processing complete')
  res_df <- toJSON(res_df, dataframe = 'columns')
  write.socket(receiver, res_df)
}

# Read in environment variables
port_val <- get_sys_env_var(var_name = 'port',var_class = 'numeric')
ed_arrival_factor <- get_sys_env_var(var_name = 'ed_arrival_factor',var_class = 'datatable')
numIters <- get_sys_env_var(var_name = 'num_replications', var_class = 'numeric')
warm_period <- get_sys_env_var(var_name = 'warm_period',var_class = 'numeric')
sim_period <- get_sys_env_var(var_name = 'sim_period',var_class = 'numeric')
num_proc <- get_sys_env_var(var_name = 'nproc',var_class = 'numeric')

# port_val <- as.numeric(readline('Enter Port number'))
if(!is.null(port_val)){
  client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)
}
res_df_list <- list()

ed_arrival_factor <- read_json_con(socket = client_socket)
ed_arrival_factor <- data.table(fromJSON(ed_arrival_factor))

origin_date <-
  as.POSIXct("2019-1-01 00:00:00", tz = "UTC") - (3600 * 24 * warm_period)
hccis <- readRDS(file.path("simulations", "function_requirements", "hccis.rds"))
# Results DF is the average number of admitted patient across all EDs
hccis_melt <-
  melt(
    copy(hccis)[, rpt_year := NULL],
    id.vars = c('hccis_id', 'owner_hccis_id', 'urban'),
    variable.name = "grouping",
    value.name = 'value'
  )[, grouping := tolower(grouping)]
hccis_melt <-
  rbind(hccis_melt, hccis_melt[, .(value = sum(value)), by = grouping][grepl('admissions', grouping) &
                                                                         !grepl('icu|ed_to_ip', grouping)][, hccis_id := 'All'], fill = TRUE)

for(train_int in seq(ceiling(numIters/num_proc))){
  tryCatch(
    expr = {
      results <- MH.Network.sim(
        rep = num_proc,
        warm = warm_period,
        sim_days = sim_period,
        ed_arrival_factor = ed_arrival_factor,
        resources = TRUE
      )
      print('Simulation complete')
      latest_res <- results[['resources']]
      if(train_int == 1){
        res_df <- latest_res
        res_df_list <- list(latest_res)
        
      } else {
        res_df_list <- c(res_df,latest_res[,replication := replication + last_rep])
        res_df <- rbindlist(res_df_list,use.names = TRUE, fill = TRUE)
      }
      last_rep <- max(latest_res$replication)
      print('Preprocessing complete')
      gen_hyperparam_res(resource_df = res_df,receiver = client_socket, warmup = warm_period)

      # print(paste('Replications',min(latest_res$replication), ' - ',max(latest_res$replication),' have completed'))
    },
    error = function(e) {
      cat('An error has occured')
      print(e)
    }
  )
  print(paste('training iteration ',train_int,' complete',sep = ''))
}