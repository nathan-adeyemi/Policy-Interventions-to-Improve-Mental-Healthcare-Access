# Read in environment variables
port_val <- get_sys_env_var(var_name = 'port', var_class = 'numeric')
numIters <-
  get_sys_env_var(var_name = 'num_replications', var_class = 'numeric')
warm_period <-
  get_sys_env_var(var_name = 'warm_period', var_class = 'numeric')
sim_period <-
  get_sys_env_var(var_name = 'sim_period', var_class = 'numeric')
num_proc <-
  get_sys_env_var(var_name = 'nproc', var_class = 'numeric')
trial_path <-
  get_sys_env_var(var_name = 'trainable_path', var_class = 'character')
tune_job <-
  get_sys_env_var(var_name = 'tune_job', var_class = 'character')
output_metric <-
  get_sys_env_var(var_name = 'output_metric', var_class = 'character')

if (!is.null(port_val)) {
  client_socket <-
    make.socket(
      host = 'localhost',
      port = as.numeric(port_val),
      server = F,
      fail = T
    )
}

sim_fn <- function(...) {
  MH.Network.sim(
    rep = num_proc,
    warm = warm_period,
    sim_days = sim_period,
    ... = ...
  )
}

arg_list = parse_tune_args(socket = client_socket, tune_job)

tryCatch(
  expr = {
    res_list <- list()
    for (train_int in seq(ceiling(numIters / num_proc))) {
      temp_results <- do.call(sim_fn, arg_list)
      if (train_int == 1) {
        results <- temp_results
      } else {
        for (i in seq(length(results))) {
          results[[i]] <- rbind(results[[i]],
                                temp_results[[i]][, replication := replication + last_rep],
                                fill = TRUE,
                                use.names = TRUE)
        }
      }
      saveRDS(results,file.path(trial_path,'raw_results.rds'))
      results_frames <-  validate_results(
        conf = 0.95,
        patients_df = results[[1]],
        resource_df = results[[2]],
        warmup = warm_period,
        sim_days = sim_period
      )
      write.xlsx(file = file.path(trial_path,'validation_frames.xlsx'), x = results_frames)
      last_rep <- max(results[[1]]$replication)
      transmit_results(
        results_list = results,
        receiver = client_socket,
        output_metric = output_metric,
        trial_path = trial_path,
        ed_df = hccis,
        warmup = warm_period,
        sim_length = sim_period,
        arg_list = arg_list
      )
    }
  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    print(e)
    # Print the line number where the error occurred
    # cat("Line number:", traceback()[[1]]$line, "\n")
  }
)
