# Read other files and functions in
siteInfo <-
  data.table(readRDS(file.path(
    "simulations",
    "function_requirements",
    "ip_facilities_info.rds"
  )))

args <- commandArgs(trailingOnly=TRUE)
if (length(args) > 0){
  port_val = args[[1]]
  client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)

  acceptance_probabilities = unique(as.data.table(fromJSON(args[[2]])))
  colnames(acceptance_probabilities)  <- c('Facility_name','Probability')
}

warm_period <- 10
sim_period <- 20

determine_params <- F

numIters <- 5
results <- MH.Network.sim(rep = numIters,
                          warm = warm_period,
                          sim_days = sim_period,
                          n.parallel = 1,
                          sort_by_prob = FALSE,
                          alg_input = NULL,
                          resources = T,
                          probs_df = NULL)



if(length(args) == 0){
  validation_frames <- validate_results(patients_df = results[['timestamps']],
                                        resource_df = results[['resources']],
                                        conf = 0.95,
                                        warmup = warm_period)

  valid_list_path <- file.path('Results','sensitivity_analysis')
  # Perform validation analysis and save results as RDS and Xlsx

  saveRDS(validation_frames,file = file.path(valid_list_path,'validation_results.rds'))
  openxlsx::write.xlsx(
    x = validation_frames,
    file = file.path(valid_list_path,'validation_results.xlsx'))
} else {

  hccis <- readRDS(file.path('Data/HCCIS/hccis.rds'))

  mean_accuracy <-
    results$resources[cap_change > 0 & is.na(patient),
                       patient := generate_random_id(13)
                        ][cap_change > 0,][siteInfo,facility := Facility_name, on = c('resource' = 'Bed_Group')
                          ][,.(admissions = length(unique(patient))),
                            by = list(facility,replication)
                            ][,.(admissions = mean(admissions)),by = facility
                              ][hccis, hccis_admissions := Total_Admissions, on = c('facility' = 'hccis_id')]

  mean_accuracy <-
    rbind(mean_accuracy, mean_accuracy[, .(
      facility = 'All',
      admissions = sum(admissions),
      hccis_admissions = sum(hccis_admissions)
    )], fill = TRUE)[, perc_diff := fifelse(test = admissions > hccis_admissions,
                                            yes = 1-(1-admissions/hccis_admissions),
                                            no = admissions / hccis_admissions)]

  mean_accuracy <- toJSON(mean_accuracy, dataframe = 'columns')
  write.socket(client_socket, mean_accuracy)
}
