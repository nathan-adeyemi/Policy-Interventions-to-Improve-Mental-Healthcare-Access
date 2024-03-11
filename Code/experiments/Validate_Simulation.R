# Read other files and functions in
siteInfo <-
  data.table(readRDS(file.path(
    "simulations",
    "function_requirements",
    "ip_facilities_info.rds"
  )))

args <- commandArgs(trailingOnly=TRUE)
if (length(args) > 0){
  port_val = args[[0]]
  client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)
  
  acceptance_probabilities = as.data.frame(fromJSON(args[[1]]))
  colnames(acceptance_probabilities)  <- c('Facility_name','Probability')
  setDT(acceptance_probabilities)
  
}

port_val <- readline('Type in port number:')
client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)
acceptance_probabilities <- read.socket(socket = client.socket)
acceptance_probabilities = as.data.frame(fromJSON(args[[1]]))
colnames(acceptance_probabilities)  <- c('Facility_name','Probability')
setDT(acceptance_probabilities)

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
  concurrent_requests = 1,
  probs_df = acceptance_probabilities
)

validation_frames <- validate_results(patients_df = results[['timestamps']], resource_df = results[['resources']], T)

if(length(args) == 0){
  valid_list_path <- file.path('Results','sensitivity_analysis')
  # Perform validation analysis and save results as RDS and Xlsx
  
  saveRDS(validation_frames,file = file.path(valid_list_path,'validation_results.rds'))
  openxlsx::write.xlsx(
    x = validation_frames,
    file = file.path(valid_list_path,'validation_results.xlsx'))
} else {
  mean_accuracy <- validation_frames$admissions_by_facility$perc_diff
}