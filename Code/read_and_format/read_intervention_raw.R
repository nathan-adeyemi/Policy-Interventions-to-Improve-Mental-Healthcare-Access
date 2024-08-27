intervention_path <- "/scratch/adeyemi.n/interventions"
int_res_df <-
  mclapply(
    X = dir(intervention_path, full.names = T),
    FUN = function(file) {
      results <- readRDS(file = file.path(file, "raw_results.rds"))
      patients <-
        results$timestamps[, treatment_delay := total_wait_time + Travel.time]
      int_res <-
        rbindlist(
          list(
            average_treatment_delay = extract_results(
              df = patients,
              metric = 'treatment_delay',
              use_type = T,
              collapse_replications = F,
              separate_vulnerable = T
            )[, `:=`(variable = 'Avg. Treatment Delay', value =  sim_val)][, sim_val := NULL],
            average_Coordination_Time =
              extract_results(
                df = patients,
                metric = 'total_wait_time',
                use_type = T,
                collapse_replications = F,
                separate_vulnerable = T
              )[, `:=`(variable = 'Avg Coordination Time', value =  sim_val)][, sim_val := NULL],
            median_treatment_delay = extract_results(
              df = patients,
              metric = 'treatment_delay',
              result_function = function(data)
                median(x = data, na.rm  = T),
              use_type = T,
              collapse_replications = F,
              separate_vulnerable = T,
            )[, `:=`(variable = 'Median Treatment Delay', value =  sim_val)][, sim_val := NULL],
            median_Coordination_Time =
              extract_results(
                df = patients,
                result_function = function(data)
                  median(x = data, na.rm  = T),
                metric = 'total_wait_time',
                use_type = T,
                collapse_replications = F,
                separate_vulnerable = T,
              )[, `:=`(
                variable = 'Median Coordination Time',
                value =  sim_val,
                sort_by_prob = grepl('TRUE', path)
              )][, sim_val := NULL]
          )
        )
      saveRDS(int_res, file = file.path(file, 'results.rds'))
      return(int_res)
    },
    mc.cores = availableCores()
  )
int_res_df <-
  rbindlist(lapply(
    X = seq_along(direct),
    FUN =  function(ind){
      int_res_df[[ind]][, sort_by_prob := grepl('True', direct[ind])]
      int_res_df[[ind]][, concurrent_requests := str_extract(direct[ind], "\\d+")]
    }
  ))
saveRDS(object = int_res_df,
        file = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Results/interventions/intervention_results.rds")
