# Read other files and functions in
siteInfo <-
  data.table(readRDS(file.path(
    "simulations",
    "function_requirements",
    "Rates5.rds"
  )))

warm_period <- 50
sim_period <- 365

determine_params <- F

numIters <- 30
results <- full_sim(
  num_iter = numIters,
  parallel = TRUE,
  warmup = warm_period,
  sim_length = sim_period,
  seed = NULL, 
  return_resources = T,
  concurrent_requests = 1
)
valid_list_path <- file.path('Results','sensitivity_analysis')
saveRDS(results,file = file.path(valid_list_path,'validation_output.rds'))

# Perform validation analysis and save results as RDS and Xlsx
validation_frames <- validate_results(patients_df = results[['timestamps']], resource_df = results[['resources']], T)
saveRDS(validation_frames,file = file.path(valid_list_path,'validation_results.rds'))
openxlsx::write.xlsx(
  x = validation_frames,
  file = file.path(valid_list_path,'validation_results.xlsx'))()