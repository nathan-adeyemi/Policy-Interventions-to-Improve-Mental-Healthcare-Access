# Set up logging and results directories/files/etc.
valid_list_path <- file.path('Results','validation','ed_factor_ED_Registrations','calc_accept')
log_path <- file.path('Code','experiments', gsub(pattern = 'Results/',replacement = '', x = valid_list_path),'logs')
file_name <- 'validation_results'

if (!dir.exists(valid_list_path)){
  dir.create(valid_list_path,recursive = TRUE)
}

if (!dir.exists(log_path)){
  dir.create(log_path,recursive = TRUE)
}

if(!interactive()){
  out_file <- file(file.path(log_path,paste0(file_name,'.out')),open = 'wt')
  err_file <- file(file.path(log_path,paste0(file_name,'.err')),open = 'wt')
  sink(file = out_file,type = 'output')
  sink(file = err_file,type = 'message')
}


# Read other files and functions in
# If running the validation script as a Slurm job, run the input paramter estimation script initially
if(!interactive()){
  source(file.path("Code","data_analysis","Simulation Input Parameters.R"))
}

numIters <- 30
warm_period <-50
sim_period <- 365

tryCatch(expr = {
results <- MH.Network.sim(rep = numIters,
                          warm = warm_period,
                          sim_days = sim_period,
                          n.parallel = 1,
                          sort_by_prob = FALSE,
                          alg_input = NULL,
                          age_frequency_input = NULL,
                          acceptance_prob_input = NULL, # Sets all acceptance probabilities to 1
                          use_registration = TRUE,
                          resources = TRUE)
saveRDS(results,file = file.path(valid_list_path, paste(file_name,'rds',sep = ".")))

},error = function(e){
  cat('An error has occured')
  print(e)
})

validation_frames <-  validate_results(
  conf = c(0.95, 0.99),
  patients_df = results[[1]],
  resource_df = results[[2]],
  warmup = warm_period,
  sim_days = sim_period
)
# Perform validation analysis and save results as RDS and Xlsx

saveRDS(
  list(
    patients = results[['timestamps']],
    resources = results[['resources']],
    validation_frames
  ),
  file = file.path(valid_list_path, paste(file_name,'rds',sep = "."))
)
openxlsx::write.xlsx(
  x = validation_frames,
  file = file.path(
    valid_list_path, paste(file_name,'xlsx',sep = ".")))
