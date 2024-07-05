# Read in environment variables
if(!interactive()){
  port_val <- get_sys_env_var(var_name = 'port', var_class = 'numeric')
  trial_path <-
    get_sys_env_var(var_name = 'trainable_path', var_class = 'character')
  tune_job <-
    get_sys_env_var(var_name = 'tune_job', var_class = 'character')
  output_metric <-
    get_sys_env_var(var_name = 'output_metric', var_class = 'character')
  seed <-
    get_sys_env_var(var_name = 'seed', var_class = 'numeric')
  sim_config_path <-
    get_sys_env_var(var_name = 'sim_config_path', var_class = 'string')
  sim_config <-
    get_sys_env_var(var_name = 'sim_config', var_class = 'string')
  trainable_name <-
    get_sys_env_var(var_name = 'trainable_name', var_class = 'string')
} else {
  port_val <- NULL
  trial_path  <- "/scratch/adeyemi.n/test"
  tune_job <- 'test'
  output_metric <- 'treatment_delay'
  seed <- 42
  sim_config_path <- "Code/experiments/configs/sim_params.yaml"
  sim_config <- "debug"
}
env <- environment()
sim_run_info = yaml::read_yaml(sim_config_path)[[sim_config]]
names(sim_run_info) <- gsub('-',"_",names(sim_run_info))
list2env(sim_run_info, env)

if (!is.null(port_val)) {
  client_socket <-
    make.socket(
      host = 'localhost',
      port = as.numeric(port_val),
      server = F,
      fail = T
    )
} else {
  client_socket <- NULL
}

sim_fn <- function(...) {
  MH.Network.sim(
    rep = num_replications,
    warm = warmup,
    sim_days = sim_length,
    seed = seed,
    ... = ...
  )
}

if(!is.null(client_socket)){
  arg_list <- parse_sim_args(socket = client_socket, tune_job)
} else { 
  arg_list <- list()
}

if (!(grepl('acceptance-probs|accept|probs',tune_job))){
    params <- fromJSON("src/simulations/function_requirements/acceptance-prob-cfg.json")[[sim_run_info$acceptance_probs]]
    arg_list$acceptance_prob_input <- data.table(Facility_name = names(params), prob = params)
}


tryCatch(
  expr = {
    res_list <- list()
    for (train_int in seq(ceiling(num_replications / parallelly::availableCores()))) {
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
        warmup = warmup,
        sim_days = sim_length
      )
      write.xlsx(file = file.path(trial_path,'validation_frames.xlsx'), x = results_frames)
      last_rep <- max(results[[1]]$replication)
      transmit_results(
        results_list = results,
        receiver = client_socket,
        output_metric = output_metric,
        trial_path = trial_path,
        ed_df = hccis,
        warmup = warmup,
        sim_length = sim_length,
        arg_list = arg_list
      )
    }

    if(grepl("incumbent",trainable_name,ignore.case = T)){
      
      print(paste("------------------------------------------------- Incumbent Validation Results -------------------------------------------------\n",results_frames),"\n")
    }
  },
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    print(e)
    # Print the line number where the error occurred
    # cat("Line number:", traceback()[[1]]$line, "\n")
  }
)
