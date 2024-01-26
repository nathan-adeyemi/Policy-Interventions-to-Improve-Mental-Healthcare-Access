rm('MH.Network.sim')
source('Simulations/Minnesota MH Network Simulation_Sensitivity_Analysis.R')
args <- commandArgs(trailingOnly=TRUE)

if(length(args) > 0){
  args <- args[1]
} else{
  args <- "ipLoS"
}

warm_period <- 50
sim_period <- 365
SA_factors <-  seq(0.5,1.5,0.1)
sa_path <- file.path('.', 'Data','Sensitivity Analysis Results')


if(!dir.exists(sa_path)){
  dir.create(sa_path)
}

sim_func <- function(vary_lambda_all = F,
                     vary_pr_accept = F,
                     vary_los = F,
                     vary_lambda_vuln = F) {
  results <- full_sim(
    num_iter = 30,
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
                         measure.vars = c('Transfer\nCoordination Time','Treatment Delay')
                         )
  
  varyLambdaPlot <-
    saPlotFun(varyLambdaTest[, value := (value / (.SD[factor == 1, value])), by = variable
                             ][, `Patient Wait Metric` := variable],
              sa_metric = 'ED Patient Arrival Rate')
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
  
  varyPrAcceptPlotData <- melt(varyPrAccept[, .(`Treatment Delay` = mean(x = TTP, na.rm = T), 
                                                            `Distance Travelled` = mean(x = Travel.Distance,na.rm = T),
                                                            perc_factor = as.factor(paste0(as.character(as.integer(factor * 100)),'%'))),
                                                        by = list(replication,factor)
                                                        ],
                         measure.vars = c('Distance Travelled','Treatment Delay')
                         )[,value := (value/(.SD[factor == 1,value])),by = variable]
  
  varyPrAcceptPlot <-
    saPlotFun(varyPrAcceptTest[, `Patient Wait Metric` := variable],
              sa_metric = 'IP Unit Likelihood to Accept Transfer')
      
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
    list(varyPrAccept, varyPrAcceptResources, varyPrAcceptTest),
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
                          )[,`:=`(vulnerable_patient = (Age != 'Adult'),placed = !is.na(total_wait_time))
                          ][,`:=`(TTP = sum(total_wait_time, Travel.time)),
                            by = list(Sim.ID, replication,factor)
                            ][,day_no := floor(IP.Arrival.Timestamp/24)]
  varyIpLoSRescources <- rbindlist(lapply(res,function(i) i[[2]]))
                
  varyIpLoSPlotData <- melt(varyIpLoSPatients[type == 'Transfer'
                                               ][, .(`Treatment Delay` = mean(x = TTP, na.rm = T), 
                                                        `Total Coordination Time` = mean(total_wait_time,na.rm = T),
                                                        `Distance Traveled` = mean(x = Travel.Distance,na.rm = T),
                                                     `% Placed in IP Care` = mean(x = placed,na.rm = T),
                                                         perc_factor = as.factor(paste0(as.character(as.integer(factor * 100)),'%'))),
                                                     by = list(replication,factor,vulnerable_patient)],
                           measure.vars = c('Distance Traveled','Treatment Delay','Total Coordination Time','% Placed in IP Care')
  )[,value := (value/(.SD[factor == 1,value])),by = list(variable,vulnerable_patient)]
  
  varyIpLoSPlot <- saPlotFun(inputData = varyIpLoSPlotData[vulnerable_patient == T,
                                           ][, `Patient Wait Metric` := variable
                                             ][variable != 'Total Coordination Time'],
                             sa_metric = 'IP Length of Stay')
    
  ggsave(
    filename = file.path(sa_path, 'LoS_analysis.jpeg'),
    plot = varyIpLoSPlot,
    width = 7,
    height = 4,
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
  
  hccis <- data.table(read_excel("Data/HCCIS/hosplist (in use).xlsx",skip = 5))[,-1]
  varyIpLoSPatients[hccis, origin_class := `MSA StatUrbans`,on = c(Site.Entered = "Hospital Name")]
  varyIpLoSPatients[is.na(origin_class) & !(Site.Entered %in% c('Children\'s Minnesota', 'Mayo Clinic Hospital - Rochester','Mercy Hospital, Unity Campus')),origin_class := 'Urban']
  varyIpLoSPatients[is.na(origin_class),origin_class := 'Rural']
  varyIpLoSPatients[siteInfo,dest_class := `classification`, on = c(Transfer.Site = 'Facility_name')]
  varyIpLoSPatients[,transfer_class_type := paste(origin_class,'->',dest_class,sep = '')]
  num_transfers <- varyIpLoSPatients[type == 'Transfer' & !is.na(Transfer.Site),.(transfer_count = .N),by = list(factor,replication)]
  std_rate <- 
    varyIpLoSPatients[factor == 1.0 & 
                      type == 'Transfer' & 
                      !is.na(Transfer.Site),.N,by = list(transfer_class_type,replication,day_no)
                    ][,.(avg_rate = mean(N)),by = list(replication,transfer_class_type)
                      ][,.(avg_rate = mean(avg_rate)),by = transfer_class_type]
  
  # Test whether there are significant differences in the rate of transfer between urban and rural facilities
  varyIpLoSTransferRates <- 
    varyIpLoSPatients[type == 'Transfer' & !is.na(Transfer.Site),.N,by = list(replication,factor,transfer_class_type,day_no)
                 ][,.(mean_rate = mean(N)), by = list(replication,factor,transfer_class_type)
                   ][std_rate, std_rate := `avg_rate`, on = c(transfer_class_type = 'transfer_class_type')
                     ][,perc_change := 100 * (std_rate - mean_rate)/std_rate
                       ][,c(reg1 = as.list(coef(lm(perc_change ~ factor))),
                            anova.1 = anova(lm(perc_change ~ factor))), by = transfer_class_type]
  #[,as.list(boot.ci(one.boot(rate,mean,500,na.rm = T))),by = list(factor,transfer_class_type)]
  
  # Test whether there are significant differences in the rate of transfer between urban and rural facilities
  varyIpLoSTransferRates2 <- 
    varyIpLoSPatients[type == 'Transfer' & !is.na(Transfer.Site),.N,by = list(replication,factor,transfer_class_type)
                      ][num_transfers, num := `transfer_count`, on = c('replication','factor')
                        ][,rate := N/num * 100, by = list(replication,factor,transfer_class_type)
                          ][,c(reg1 = as.list(coef(lm(rate ~ factor + transfer_class_type))),
                               anova.1 = anova(lm(rate ~ factor + transfer_class_type)))]

  lagged_occupancy <- 
    varyIpLoSResources[siteInfo,urban_class := `classification`, on = c(resource = 'Bed_Group')
                        ][,`:=`(lag_diff = c(NA,diff(time)),
                                occupancy = 100 * server/capacity),by = list(replication,resource,factor)
                          ][!is.na(lag_diff),.(occupancy = weighted.mean(x = occupancy, w = lag_diff, na.rm = T),
                               urban_class = unique(urban_class)), by = list(replication,resource,factor)
                            ]
  ipUnitTests <- lagged_occupancy[,c(urban_class = unique(urban_class),
                                 reg1 = as.list(coef(lm(occupancy ~ factor))),
                                 anova.1 = as.list(anova(lm(occupancy ~ factor)))), by = resource]
  urbanTests <- lagged_occupancy[,c(reg1 = as.list(coef(lm(occupancy ~ urban_class + factor + urban_class:factor))))]
  
  saveRDS(
    list(varyIpLoSPatients,varyIpLoSPlotData,varyIpLoSRescources, varyIpLoSTTP,varyIpLoSTransferRates),
    file.path(sa_path, 'Length of Stay Sensitivity Analysis Results.rds')
  )
}