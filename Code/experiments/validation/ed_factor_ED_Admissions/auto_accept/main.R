# Set up logging and results directories/files/etc.
valid_list_path <- file.path('Results','validation','ed_factor_ED_Admissions','auto_accept')
log_path <- file.path('Code','experiments','logs','ed_factor_ED_Admissions','auto_accept')
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

# numIters <- 30
# warm_period <-50
# sim_period <- 365

numIters <- 5
warm_period <- 10
sim_period <- 25

tryCatch(expr = {
results <- MH.Network.sim(rep = numIters,
                          warm = warm_period,
                          sim_days = sim_period,
                          concurrent_requests = 1,
                          # acceptance_prob_input = unique(siteInfo[,list(Facility_name)])[,prob := 1], # Sets all acceptance probabilities to 1
                          resources = TRUE)
# saveRDS(results,file = file.path(valid_list_path, paste(file_name,'rds',sep = ".")))

},error = function(e){
  cat('An error has occured')
  print(e)
})

validation_frames <-  validate_results(
  conf = 0.95,
  patients_df = results[[1]],
  resource_df = results[[2]],
  warmup = warm_period,
  sim_days = sim_period
)

browser()
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

# Print Validation results to the .out file
if(!interactive()){
  print('Validation Results \n')
  print(validation_frames)
}
