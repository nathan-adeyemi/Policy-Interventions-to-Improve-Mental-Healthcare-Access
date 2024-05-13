# Read in environment variables
port_val <- get_sys_env_var(var_name = 'port',var_class = 'numeric')
numIters <- get_sys_env_var(var_name = 'num_replications', var_class = 'numeric')
warm_period <- get_sys_env_var(var_name = 'warm_period',var_class = 'numeric')
sim_period <- get_sys_env_var(var_name = 'sim_period',var_class = 'numeric')
num_proc <- get_sys_env_var(var_name = 'nproc',var_class = 'numeric')
trial_path <- get_sys_env_var(var_name='trainable_path',var_class='character')
tune_job <- get_sys_env_var(var_name='tune_job',var_class='character')

if(!is.null(port_val)){
  client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)
}

sim_fn <- function(...){
  MH.Network.sim(
        rep = num_proc,
        warm = warm_period,
        sim_days = sim_period,
        ... = ...
      )
}

arg_list = parse_tune_args(socket = client_socket,tune_job)

tryCatch(
    expr = {
      res_list <- list()
      for(train_int in seq(ceiling(numIters/num_proc))){
        temp_results <- do.call(sim_fn, arg_list)
        if(train_int == 1){
          results <- temp_results
        } else {
          for(i in seq(length(results))){
            results[[i]] <- rbind(results[[i]], 
                                  temp_results[[i]][,replication := replication + last_rep],
                                  fill = TRUE,
                                  use.names = TRUE)
          }
        }
        last_rep <- max(results[[1]]$replication)

        if(tune_job %in% c('ed-arr','ed-arr-sep','accept-prob')){
          validation_frames <-  validate_results(
            conf = 0.95,
            patients_df = results[[1]],
            resource_df = results[[2]],
            warmup = warm_period,
            sim_days = sim_period
          )
          write.xlsx(file = file.path(trial_path,'validation_frames.xlsx'), x = validation_frames)
          res_df <- validation_frames$admissions_by_facility
        } else {
          # saveRDS(results,file.path(trial_path,'raw_results.rds'))
          res_df <- results[[1]][, treatment_delay := total_wait_time + Travel.time
                                ][,.(`Coordination Time` = one.boot(total_wait_time,FUN=mean,R=500,na.rm=TRUE)$t0,
                                     `Treatment Delay` = one.boot(treatment_delay,FUN=mean,R=500,na.rm=TRUE)$t0),
                                    by = list(replication,type,`Vulnerable Patient` = Age != 'Adult')]
          for(arg in names(arg_list)){
            res_df[,(arg) := arg_list[arg]]
          }
        }
        transmit_results(res_df = res_df, receiver = client_socket, tune_job=tune_job)
      }
    },
  error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        print(e)        
        # Print the line number where the error occurred
        # cat("Line number:", traceback()[[1]]$line, "\n")
      })
