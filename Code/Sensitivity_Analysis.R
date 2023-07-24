rm('MH.Network.sim')
source('Simulations/Minnesota MH Network Simulation_Sensitivity_Analysis.R')
args <- commandArgs(trailingOnly=TRUE)

if(length(args) > 0){
  args <- args[[1]]
} else{
  args <- "ipLoS"
}

warm_period <- 30
sim_period <- 365
SA_factors <-  seq(0.5,1.5,0.1)
sa_path <- file.path('.',
                     'Data',
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
    return_resources = T,
    multipliers = SA_factors, 
    all_lambda_vary = vary_lambda_all,
    pr_accept_vary = vary_pr_accept,
    los_vary = vary_los)
  return(results)
}
# Varying the ED->IP Arrival Rate for everyone ----------------------------
if(args == "edToIpLambda"){
  res <- sim_func(vary_lambda_all = T)
  
  varyLambdaPatients <- rbindlist(lapply(res,function(df) df[[1]]))
  varyLambdaResources <- rbindlist(lapply(res,function(df) df[[2]]))

  varyLambdaTest <- melt(varyLambdaPatients[,vulnerable_patient := (Age != 'Adult')
                             ][,`:=`(TTP = sum(total_wait_time, Travel.time)),
                               by = list(Sim.ID, replication,factor)
                               ][, .(`Treatment Delay` = mean(x = TTP, na.rm = T), 
                                     `Transfer\nCoordination Time` = mean(x = total_wait_time,na.rm = T),
                                     perc_factor = as.factor(paste0(as.character(as.integer(factor * 100)),'%'))),
                                 by = list(replication,factor)],
                                # ][, .(`Transfer\nCoordination Time` = mean(total_wait_time),`Treatment Delay` = mean(TTP)), by = factor],
                         measure.vars = c('Transfer\nCoordination Time','Treatment Delay')
                         )
  
  varyLambdaPlot <-
    saPlotFun(varyLambdaTest[, value := (value / (.SD[factor == 1, value])), by = variable
                             ][, `Patient Wait Metric` := variable])
  ggsave(
    filename = file.path(sa_path, 'arrival_rate_sensitivity_analysis.jpeg'),
    plot = varyLambdaPlot,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  varyLambdaTTP <-
    mclapply(
      X = SA_factors,
      FUN = function(value)
        extract_results(
          df = varyLambdaPatients[factor == value,],
          metric = 'TTP',
          use_type = T,
          separate_vulnerable = T
        ),
      mc.cores = length(SA_factors)
    )
  saveRDS(
    list(varyLambdaPatients, varyLambdaResources,varyLambdaTTP),
    file.path(sa_path, 'Arrival Rate Sensitivity Analysis Results.rds')
  )
}

# Varying the Pr{Accepted by Facility} ------------------------------------
if(args == "ipAcceptance"){
  res <- sim_func(vary_pr_accept = T)
  
  varyPrAccept <- rbindlist(lapply(res,function(i) i[[1]])
                            )[type == 'Transfer'
                              ][,vulnerable_patient := (Age != 'Adult')
                                ][,`:=`(TTP = sum(total_wait_time, Travel.time)),
                                  by = list(Sim.ID, replication,factor)]
  
  varyPrAcceptResources <- rbindlist(lapply(res,function(i) i[[2]]))
  
  varyPrAcceptTest <- melt(varyPrAccept[, .(`Treatment Delay` = mean(x = TTP, na.rm = T), 
                                                            `Distance Travelled` = mean(x = Travel.Distance,na.rm = T),
                                                            perc_factor = as.factor(paste0(as.character(as.integer(factor * 100)),'%'))),
                                                        by = list(replication,factor)
                                                        ],
                         measure.vars = c('Distance Travelled','Treatment Delay')
                         )[,value := (value/(.SD[factor == 1,value]) * 100),by = variable]
  
  varyPrAcceptPlot <- saPlotFun(varyPrAcceptTest[, `Patient Wait Metric` := variable])
      
  ggsave(
    filename = file.path(sa_path, 'acceptance_rate_analysis.jpeg'),
    plot = varyPrAcceptPlot,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  varyPrAcceptTest <-
    mclapply(
      X = SA_factors,
      FUN = function(value)
        extract_results(
          df = varyPrAccept[factor == value,],
          metric = 'TTP',
          use_type = T,
          separate_vulnerable = T
        ),
      mc.cores = length(SA_factors)
    )
  saveRDS(
    list(varyPrAccept, varyPrAcceptResources, varyPr varyPrAcceptTest),
    file.path(sa_path, 'Transfer Acceptance Rate Sensitivity Analysis Results.rds')
  )
  
}

# Varying the Inpatient Length of stays -----------------------------------
if(args == "ipLoS"){

  if (!dir.exists(file.path('.','Data','Sensitivity Analysis Results','los_temp_folder'))){
    dir.create(
      file.path(
        '.',
        'Data',
        'Sensitivity Analysis Results',
        'los_temp_folder'
      )
    )
  }
  
  res <- sim_func(vary_los = T)
  varyIpLoSPatients <-  rbindlist(lapply(res,function(i) i[[1]])
                          )[,vulnerable_patient := (Age != 'Adult')
                          ][,`:=`(TTP = sum(total_wait_time, Travel.time)),
                            by = list(Sim.ID, replication,factor)]
  varyIpLoSRescources <- rbindlist(lapply(res,function(i) i[[2]]))
                
  varyIpLoSTest <- melt(varyIpLoSPatients[type == 'Transfer'
                                               ][, .(`Treatment Delay` = mean(x = TTP, na.rm = T), 
                                                        `Total Coordination Time` = mean(total_wait_time,na.rm = T),
                                                        `Distance Traveled` = mean(x = Travel.Distance,na.rm = T),
                                                         perc_factor = as.factor(paste0(as.character(as.integer(factor * 100)),'%'))),
                                                     by = list(replication,factor,vulnerable_patient)],
                           measure.vars = c('Distance Traveled','Treatment Delay','Total Coordination Time')
  )[,value := (value/(.SD[factor == 1,value]) * 100),by = list(variable,vulnerable_patient)]
  
  varyIpLoSPlot <- saPlotFun(varyIpLoSTest[vulnerable_patient == T,
                                           ][, `Patient Wait Metric` := variable
                                             ][variable != 'Total Coordination Time'])
    
  ggsave(
    filename = file.path(sa_path, 'LoS_analysis.jpeg'),
    plot = varyIpLoSPlot,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  varyIpLoSTTP <-
    mclapply(
      X = SA_factors,
      FUN = function(value)
        extract_results(
          df = varyIpLoSPatients[factor == value,],
          metric = 'TTP',
          use_type = T,
          separate_vulnerable = T
        ),
      mc.cores = length(SA_factors)
    )
  saveRDS(
    list(varyIpLoSPatients,varyIpLoSTest,varyIpLoSRescources, varyIpLoSTTP),
    file.path(sa_path, 'Length of Stay Sensitivity Analysis Results.rds')
  )
}