# Read other files and functions in
siteInfo <-
  data.table(readRDS(file.path(
    "simulations",
    "function_requirements",
    "ip_facilities_info.rds"
  )))

warm_period <- 50
sim_period <- 365

determine_params <- F
tic()
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
toc()
valid_list_path <- file.path('Results','validation')

saveRDS(results,file = file.path(valid_list_path,'validation_output.rds'))

patients = rbindlist(lapply(results,function(i) i$timestamps))
resources = rbindlist(lapply(results,function(i) i$resources))

saveRDS(list(patients = patients, resources = resources),file = file.path(valid_list_path,'validation_output.rds'))

# Perform validation analysis and save results as .RDS and .xlsx
validation_frames <-
  validate_results(patients_df = patients, 
                   resource_df = resources,
                   conf = 0.99,
                   warmup = warm_period)

saveRDS(validation_frames,file = file.path(valid_list_path,'validation_results.rds'))
openxlsx::write.xlsx(
  x = validation_frames,
  file = file.path(valid_list_path,'validation_results.xlsx'))

