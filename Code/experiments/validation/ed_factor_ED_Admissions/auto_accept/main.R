# Set up logging and results directories/files/etc.
valid_list_path <- file.path('Results','validation','auto_accept')
log_path <- file.path('Code','experiments','logs')
file_name <- 'best_ed_scale_param'

if (!dir.exists(valid_list_path)){
  dir.create(valid_list_path,recursive = TRUE)
}

if (!dir.exists(log_path)){
  dir.create(log_path,recursive = TRUE)
}

sink(file = file(file.path(log_path,paste0(file_name,'.out')),open = 'wt'),type = 'output')
sink(file =  file(file.path(log_path,paste0(file_name,'.err')),open = 'wt'),type = 'message')


numIters <- 30
warm_period <-50
sim_period <- 365


tryCatch(expr = {
results <- MH.Network.sim(rep = numIters,
                          warm = warm_period,
                          sim_days = sim_period,
                          acceptance_prob_input = unique(siteInfo[,list(Facility_name)])[,prob := 1], # Sets all acceptance probabilities to 1
                          ed_scale_param = 1.22,
                          resources = TRUE)

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

closeAllConnections()
