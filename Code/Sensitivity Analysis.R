source("functions.R")
source('Minnesota MH Network Simulation_Sensitivity_Analysis.R')
siteInfo <-
  data.table(readRDS(file.path(
    ".",
    "Data",
    "Function Requirements",
    "Rates5.rds"
  )))
warm_period <- 30
sim_period <- 200
SA_factors <-  seq(0.5,1.5,0.1)
sa_path <- file.path('.',
                     'Simulation and Alternatives',
                     'Sensitivity Analysis Results')

if(!dir.exists(sa_path)){
  dir.create(sa_path)
}

sim_func <- function(vary_lambda_all = F,
                     vary_pr_accept = F,
                     vary_los = F,
                     vary_lambda_vuln = F) {
  results <- full_sim(
    num_iter = 20,
    parallel = TRUE,
    warmup = warm_period,
    sim_length = sim_period,
    save_files = F,
    return_resources = F,
    multipliers = SA_factors, 
    all_lambda_vary = vary_lambda_all,
    pr_accept_vary = vary_pr_accept,
    los_vary = vary_los)
  return(results)
}

# vary_all_EDtoIP_lambda <- readline(prompt = "Change ED -> IP arrival rates?:")
# vary_ip_acceptance <- readline(prompt = "Change IP acceptance rate?:")
# vary_vuln_EDtoIP_lambda <- readline(prompt = "Change ED -> IP arrival rate for vulnerable patients?:")
# vary_ip_LoS <- readline(prompt = "Vary IP length of stays?:")

vary_all_EDtoIP_lambda <- F
vary_ip_acceptance <- F
vary_vuln_EDtoIP_lambda <- F
vary_ip_LoS <- T

# Varying the ED->IP Arrival Rate for everyone ----------------------------
if(vary_all_EDtoIP_lambda){
  SA_vary_all_lambda_results <- sim_func(vary_lambda_all = T)
  
  SA_lambda_test <- melt(SA_vary_all_lambda_results[,vulnerable_patient := (Age != 'Adult')
                             ][,`:=`(TTP = sum(total_wait_time, Travel.time)),
                               by = list(Sim.ID, replication,factor)
                               ][, .(TTP = mean(x = TTP, na.rm = T), 
                                     total_wait_time = mean(x = total_wait_time,na.rm = T),
                                     perc_factor = as.factor(paste0(as.character(as.integer(factor * 100)),'%'))),
                                 by = list(replication,factor)
                                 ][, .(`Transfer Coordination Time` = mean(total_wait_time),
                                       `Treatment Delay` = mean(TTP)), by = factor],
                         measure.vars = c('Transfer Coordination Time','Treatment Delay')
                         )
  
  SA_lambda_plot <-
    ggplot(
      data = SA_lambda_test[,value := (value/(.SD[factor == 1,value])),by = variable
                            ][, `Patient Wait Metric` := variable],
      mapping = aes(
        x = factor,
        y = value,
        group = `Patient Wait Metric`,
        linetype = `Patient Wait Metric`
      )
    ) + geom_point() + geom_smooth() +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Percentage of Baseline ED Arrival Rate') +
    ylab('% of Baseline Simulation\'s\nWait Time (in hrs.)') +
    guides(linetype = guide_legend(title = "Patient Wait Type")) 
  
  ggsave(
    filename = file.path(sa_path, 'arrival_rate_sensitivity_analysis.jpeg'),
    plot = SA_lambda_plot,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  SA_vary_all_lambda_TTP <-
    mclapply(
      X = SA_factors,
      FUN = function(value)
        extract_results(
          df = SA_vary_all_lambda_results[factor == value,],
          metric = 'TTP',
          use_type = T,
          separate_vulnerable = T
        ),
      mc.cores = length(SA_factors)
    )
  saveRDS(
    list(SA_vary_all_lambda_results, SA_vary_all_lambda_TTP),
    file.path(sa_path, 'Arrival Rate Sensitivity Analysis Results.rds')
  )
}

# Varying the Pr{Accepted by Facility} ------------------------------------
if(vary_ip_acceptance){
  SA_vary_pr_accept_results<- sim_func(vary_pr_accept = T)
  
  SA_pr_accept_test <- melt(SA_vary_pr_accept_results[type == 'Transfer'][,vulnerable_patient := (Age != 'Adult')
                                                    ][,`:=`(TTP = sum(total_wait_time, Travel.time)),
                                                      by = list(Sim.ID, replication,factor)
                                                      ][, .(TTP = mean(x = TTP, na.rm = T), 
                                                            Travel.Distance = mean(x = Travel.Distance,na.rm = T),
                                                            perc_factor = as.factor(paste0(as.character(as.integer(factor * 100)),'%'))),
                                                        by = list(replication,factor)
                                                        ][, .(`Distance Travelled` = mean(Travel.Distance),
                                                              `Treatment Delay` = mean(TTP)), by = factor],
                         measure.vars = c('Distance Travelled','Treatment Delay')
                         )[,value := (value/(.SD[factor == 1,value]) * 100),by = variable]
  SA_pr_accept_plot <-
    ggplot(
      data = SA_pr_accept_test[, `Patient Wait Metric` := variable],
      mapping = aes(
        x = factor,
        y = value,
        group = `Patient Wait Metric`,
        linetype = `Patient Wait Metric`
      )
    ) + geom_point() + geom_smooth() +
    scale_x_continuous(labels = scales::percent_format()) + 
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    xlab('Percentage of Baseline ED Arrival Rate') +
    ylab('% of Baseline Simulation\'s Value') +
    guides(fill = guide_legend(title = "Patient Wait Metric"))
  
  ggsave(
    filename = file.path(sa_path, 'acceptance_rate_analysis.jpeg'),
    plot = SA_lambda_plot,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  SA_vary_pr_accept_TTP <-
    mclapply(
      X = SA_factors,
      FUN = function(value)
        extract_results(
          df = SA_vary_pr_accept_results[factor == value,],
          metric = 'TTP',
          use_type = T,
          separate_vulnerable = T
        ),
      mc.cores = length(SA_factors)
    )
  saveRDS(
    list(SA_vary_pr_accept_results, SA_vary_pr_accept_TTP),
    file.path(sa_path, 'Transfer Acceptance Rate Sensitivity Analysis Results.rds')
  )
  
}

# Varying the Inpatient Length of stays -----------------------------------
if(vary_ip_LoS){

  if (!dir.exists(file.path('.','Simulation and Alternatives','Sensitivity Analysis Results','los_temp_folder'))){
    dir.create(
      file.path(
        '.',
        'Simulation and Alternatives',
        'Sensitivity Analysis Results',
        'los_temp_folder'
      )
    )
  }
  
  SA_vary_LoS_results <- sim_func(vary_los = T)
  
  unlink(
    file.path(
      '.',
      'Simulation and Alternatives',
      'Sensitivity Analysis Results',
      'temp_folder'
    ),
    recursive = T
  )
  
  SA_vary_LoS_test <- melt(SA_vary_LoS_results[type == 'Transfer'
                                               ][,vulnerable_patient := (Age != 'Adult')
                                                 ][,`:=`(TTP = sum(total_wait_time, Travel.time)),
                                                   by = list(Sim.ID, replication,factor)
                                                   ][, .(TTP = mean(x = TTP, na.rm = T), 
                                                        total_wait_time = mean(total_wait_time,na.rm = T),
                                                         Travel.Distance = mean(x = Travel.Distance,na.rm = T),
                                                         perc_factor = as.factor(paste0(as.character(as.integer(factor * 100)),'%'))),
                                                     by = list(replication,factor,vulnerable_patient)
                                                     ][, .(`Distance Traveled` = mean(Travel.Distance),
                                                           `Treatment Delay` = mean(TTP),
                                                           `Total Coordination Time` = mean(total_wait_time,na.rm = T)), 
                                                       by = list(vulnerable_patient,factor)],
                           measure.vars = c('Distance Traveled','Treatment Delay','Total Coordination Time')
  )[,value := (value/(.SD[factor == 1,value]) * 100),by = list(variable,vulnerable_patient)]
  
  SA_vary_LoS_plot <-
    ggplot(
      data = SA_vary_LoS_test[vulnerable_patient == T,
                              ][, `Patient Metric` := variable
                               ][variable != 'Total Coordination Time'],
      mapping = aes(
        x = factor,
        y = value,
        group = `Patient Metric`,
        linetype = `Patient Metric`
      )
    ) + geom_point() + geom_smooth() +
    scale_x_continuous(labels = scales::percent_format()) + 
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    xlab('Percentage of Patient\'s Sampled Length of Stay') +
    ylab('% of Baseline Simulation\'s Value') +
    guides(fill = guide_legend(title = "Patient Metric"))
  
  ggsave(
    filename = file.path(sa_path, 'LoS_analysis.jpeg'),
    plot = SA_vary_LoS_plot,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  SA_vary_LoS_TTP <-
    mclapply(
      X = SA_factors,
      FUN = function(value)
        extract_results(
          df = SA_vary_LoS_results[factor == value,],
          metric = 'TTP',
          use_type = T,
          separate_vulnerable = T
        ),
      mc.cores = length(SA_factors)
    )
  saveRDS(
    list(SA_vary_LoS_results, SA_vary_LoS_TTP),
    file.path(sa_path, 'Length of Stay Sensitivity Analysis Results.rds')
  )
}

# # Varying the ED->IP Arrival Rate for Vulnerable Patients -----------------
# if(vary_vuln_EDtoIP_lambda){
#   SA_vary_vuln_lambda_results <- sim_func(vary_lambda_vuln = T)
#   
#   SA_vary_vuln_lambda_TTP <-
#     mclapply(
#       X = SA_factors,
#       FUN = function(value)
#         extract_results(
#           df = SA_vary_vuln_lambda_results[factor == value,],
#           metric = 'TTP',
#           use_type = T,
#           separate_vulnerable = T
#         ),
#       mc.cores = length(SA_factors)
#     )
#   saveRDS(
#     list(SA_vary_vuln_lambda_results, SA_vary_vuln_lambda_TTP),
#     file.path(sa_path, 'Length of Stay Sensitivity Analysis Results.rds')
#   )
# }
