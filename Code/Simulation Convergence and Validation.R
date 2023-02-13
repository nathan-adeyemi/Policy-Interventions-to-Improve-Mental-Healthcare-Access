# Read other files and functions in
source("functions.R")
source('Minnesota MH Network Simulation.R')
siteInfo <-
  data.table(readRDS(file.path(
    ".",
    "Data",
    "Function Requirements",
    "Rates5.rds"
  )))

warm_period <- 30
sim_period <- 200

determine_params <- F
reap_zombies()

# Run Simulation for many reps with a long sim length ---------------------
simulate_or_read.data <- readline(prompt = "Read data files from last run?:")
# simulate_or_read.data <- FALSE #Automatically run new simulation trials
simulate_or_read.data <- as.logical(simulate_or_read.data)

valid_list_path <- file.path(".", "Simulation and Alternatives", "Validation Results") %>%
  file.path(., list.files(.)[length(list.files(.))]) %>%
  file.path(., list.files(.)[length(list.files(.))], "Validation Metric Compairisons")

if (!simulate_or_read.data) {
  temp_folder <- file.path(
    ".",
    "Simulation and Alternatives",
    "Validation Results",
    paste("temp_results", format(Sys.Date(), "%m_%d"), sep = "_")
  )


  if (!dir.exists(temp_folder)) {
    dir.create(temp_folder)
  }

  numIters <- 20
  results <- full_sim(
    num_iter = numIters,
    parallel = TRUE,
    warmup = warm_period,
    sim_length = sim_period,
    seed = NULL, 
    return_resources = T,
    concurrent_requests = 1
  )
  valid_list_path <- file.path(".", "Simulation and Alternatives", "Validation Results") %>%
    file.path(., list.files(.)[length(list.files(.))]) %>%
    file.path(., list.files(.)[length(list.files(.))], "Validation Metric Compairisons")
  validation_frames <-
    validate_results(patients_df = results[['timestamps']], 
                     resource_df = results[['resources']], T)
  saveRDS(validation_frames,file = paste0(valid_list_path,'.rds'))
  openxlsx::write.xlsx(
    x = validation_frames,
    file = paste0(
      valid_list_path,
      '_',
      gsub(x = Sys.Date(), pattern = '-', '_'),
      '_',
      paste(
        'Trial',
        length(
          list.files(
            file.path(".", "Simulation and Alternatives", "Validation Results") %>% 
              file.path(., list.files(.)[length(list.files(.))]))),sep = '_'),
      '.xlsx'),overwrite = F)
  
} else {
  if ("Validation Metric Compairisons.rds" %in% list.files(file.path(".", "Simulation and Alternatives", "Validation Results") %>%
    file.path(., list.files(.)[length(list.files(.))]) %>%
    file.path(., list.files(.)[length(list.files(.))]))) {
    validation_frames <- readRDS(file = paste(file.path(valid_list_path),'rds',sep = "."))
    openxlsx::write.xlsx(
      x = validation_frames,
      file = paste0(
        valid_list_path,
        '_',
        gsub(x = Sys.Date(), pattern = '-', '_'),
        '_',
        paste(
          'Trial',
          length(
            list.files(
              file.path(".", "Simulation and Alternatives", "Validation Results") %>% 
                file.path(., list.files(.)[length(list.files(.))]))),sep = '_'),
        '.xlsx'),overwrite = F)
  } else {
    patients <- readRDS(file.path(file.path(".","Simulation and Alternatives", "Validation Results") %>%
      file.path(., list.files(.)[length(list.files(.))]) %>%
      file.path(., list.files(.)[length(list.files(.))], "Patients Results (All Replications).rds")))
    
    resources <- readRDS(file.path(file.path(".","Simulation and Alternatives", "Validation Results") %>%
      file.path(., list.files(.)[length(list.files(.))]) %>%
      file.path(., list.files(.)[length(list.files(.))], "Resource Results (All Replications).rds")))
    
    if(is.list(patients)){
      patients <- patients[[1]]
    }
    
    if(is.list(resources)){
      resources <- resources[[1]]
    }

    setorder(patients, replication, Enter.Timestamp)
    validation_frames <- validate_results(patients, resources, T)
    
    saveRDS(validation_frames,file = paste0(valid_list_path,'.rds'))
    openxlsx::write.xlsx(
      x = validation_frames,
      file = paste0(
        valid_list_path,
        '_',
        gsub(x = Sys.Date(), pattern = '-', '_'),
        '_',
        paste(
          'Trial',
          length(
            list.files(
              file.path(".", "Simulation and Alternatives", "Validation Results") %>% 
                file.path(., list.files(.)[length(list.files(.))]))),sep = '_'),
        '.xlsx'),overwrite = F)
    # plot_usage(resources, patients)
  }
}

patients <- patients[,sim_day := sim_date(3600*Enter.Timestamp)]
day_rep_means <- patients[,.(TTP = one.boot(total_wait_time,mean,500,na.rm = T)$t0),by = list(sim_day,replication)]
p_vals <- c()
min_conf = 0.05
for (i in 75:max(day_rep_means[,sim_day])){
  p_vals[length(p_vals)+1] = t.test(day_rep_means[sample(.N,1000)][sim_day <= i,TTP],day_rep_means[sample(.N,2000),TTP])$p.value
}
min_days = which(p_vals < min_conf)[1] + min(day_rep_means[,sim_day])
