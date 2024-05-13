args <- commandArgs(trailingOnly=TRUE)

if(length(args) > 0){
  args <- args[1]
} else{
  args <- "run_int_3"
}

# Full Experiment Simulation Parameters (replications, warmup, etc)
numIters <- 30
warm_period <- 50
sim_period <- 365
results_path <-
  file.path(
    '.',
    'Data',
    ifelse(test = args == 'run_baseline',yes = 'Baseline Results',no = 'Intervention Results')
  )

int_2and3_concurrent <- 8
sim_func <- function(sort_by_prob, 
                     concurrent_requests,
                     resources = F,
                     test_concurrent = F) {
  results <- full_sim(
    num_iter = numIters,
    parallel = TRUE,
    prob_sort = sort_by_prob,
    warmup = warm_period,
    concurrent_requests = concurrent_requests,
    sim_length = sim_period,
    save_files = F,
    return_resources = resources,
    rep_parallel_combo = test_concurrent)
  return(results)
}

format_sim_patient_data <- function(df,FUN) {
  return(df[, `:=`(TTP = rowSums(.SD,na.rm=T)),.SDcols = c('total_wait_time','Travel.time')
            ][,`:=`(vulnerable_patient = (Age != 'Adult')), by = list(Sim.ID, replication,n.concurrent,type)
             ][TTP > 0,lapply(.SD,FUN,na.rm = T), .SDcols = c('TTP','total_wait_time','Travel.Distance'), by = list(replication,n.concurrent,type,vulnerable_patient)])
}


if(!dir.exists(results_path)){
  dir.create(results_path)
}

# Baseline Results --------------------------------------------------------
if(args == 'run_baseline'){
  
  res <-
    sim_func(sort_by_prob = F,
             concurrent_requests = 1,
             resources = T)
  
  baseline_patients <- rbindlist(lapply(res,function(df) df[[1]])
                                 )[, `:=`(TTP = sum(total_wait_time, 
                                                    Travel.time)), 
                                   by = list(Sim.ID,replication)]
  baseline_resources <- rbindlist(lapply(res,function(df) df[[2]]))
  
  baseline_results = list(
    average_TTP = extract_results(
      df = baseline_patients,
      metric = 'TTP',
      use_type = T,
      separate_vulnerable = T
    ),
    average_Coordination_Time = 
      extract_results(
        df = baseline_patients,
      metric = 'total_wait_time',
      use_type = T,
      separate_vulnerable = T
    ), 
    percent_transfer = extract_results(
      df = baseline_patients[, is_transfer := type == 'Transfer'],
      metric = 'is_transfer',
      use_type = F,
      separate_vulnerable = T
    ),
    transfer_within_10 = extract_results(
      df = baseline_patients[type == 'Transfer'][, `:=`(in_range = Travel.Distance <= 10)],
      metric = 'in_range',
      use_type = F,
      separate_vulnerable = T
    ),
    transfer_within_25 = extract_results(
      df = baseline_patients[type == 'Transfer'][, `:=`(in_range = Travel.Distance <= 25 & Travel.Distance > 10)],
      metric = 'in_range',
      use_type = F,
      separate_vulnerable = T
    ),
    transfer_within_50 = extract_results(
      df = baseline_patients[type == 'Transfer'
                             ][, `:=`(in_range = Travel.Distance <= 50 & Travel.Distance > 25)],
      metric = 'in_range',
      use_type = F,
      separate_vulnerable = T
    ),
    transfer_within_50 = extract_results(
      df = baseline_patients[type == 'Transfer'
                             ][, `:=`(in_range = Travel.Distance > 50 )],
      metric = 'in_range',
      use_type = F,
      separate_vulnerable = T
    ),
    ip_occupancy = extract_results(
      df = baseline_resources[,`:=`(Occupancy = 100 * (server/capacity))],
      metric = 'Occupancy',
      use_type = F,
      separate_vulnerable = F,
      resource = T
    )
  )

  
                                  
  saveRDS(
    list(baseline_patients, baseline_resources,baseline_results),
    file = file.path(results_path, 'baseline_data_and_results.rds')
  )
  
  saveRDS(baseline_patients,file = file.path("Data","Intervention Results","baseline_test_data.rds"))
  openxlsx::write.xlsx(baseline_results,
                       file = file.path(results_path, 'Baseline_Processed_Results.xlsx'))
  rm(list = c('baseline_resources'))
}

# Intervention 1: Patients routed by Pr{Facility Acceptance} --------------
if(args == 'run_int_1') {
  results_path <- file.path(results_path,'Intervention_1')

  if(!dir.exists(results_path)){
    dir.create(results_path)
  }

  res <-
    sim_func(sort_by_prob = T,
             n.concurrent = 1,
             resources = T)
  
  int_1_patients <- rbindlist(lapply(res,function(df) df[[1]])
                              )[, `:=`(TTP = sum(total_wait_time, Travel.time)), by = Sim.ID]
  int_1_resources <- rbindlist(lapply(res,function(df) df[[2]]))
  
  
  intervention_1_TTP <-
    extract_results(
      df = int_1_patients,
      metric = 'TTP',
      use_type = T,
      separate_vulnerable = T
    )
  
  int_1_test_data <- format_sim_patient_data(rbind(int_1_patients,baseline_patients,fill = T),mean)
  
  
  int_1_tests  <-  list(
      treatment_delay = wilcox.test(formula = TTP ~ n.concurrent,  data = int_1_test_data[vulnerable_patient == T & type == 'Transfer'],na.rm = TRUE,paired = FALSE,exact = FALSE,conf.int = TRUE),
      treatment_delay_1_sides = wilcox.test(x = int_1_test_data[vulnerable_patient == T & type == 'Transfer' & n.concurrent == 'Baseline',TTP], int_1_test_data[vulnerable_patient == T & type == 'Transfer' & n.concurrent == 'Intervention_1',TTP] ,na.rm = TRUE,paired = FALSE,exact = FALSE,conf.int = TRUE,alternative = 'g'),
      coordination_time = wilcox.test(formula = total_wait_time ~ n.concurrent, data = int_1_test_data[vulnerable_patient == T & type == 'Transfer'],na.rm = TRUE,paired = FALSE,exact = FALSE,conf.int = TRUE),
      treatment_delay_adult = wilcox.test(TTP ~ n.concurrent,data = int_1_test_data[type == 'Transfer' & vulnerable_patient == F],na.rm = TRUE,paired = FALSE,exact = FALSE,conf.int = TRUE),
      coordination_time_adult = wilcox.test(total_wait_time ~ n.concurrent,data = int_1_test_data[type == 'Transfer' & vulnerable_patient == F],na.rm = TRUE,paired = FALSE,exact = FALSE,conf.int = TRUE),
      transfer_distance_vulnerable = wilcox.test(Travel.Distance ~ n.concurrent,data = int_1_test_data[vulnerable_patient == T &type == 'Transfer'], na.rm = TRUE,paired = FALSE,exact = FALSE,conf.int = TRUE),
      transfer_distance_adult = wilcox.test(Travel.Distance ~ n.concurrent,data = int_1_test_data[vulnerable_patient == F &type == 'Transfer'], na.rm = TRUE,paired = FALSE,exact = FALSE,conf.int = TRUE),
      treatment_delay_internal = wilcox.test(TTP ~ n.concurrent,data = int_1_test_data[type == 'Internal'],na.rm = TRUE,paired = FALSE,exact = FALSE,conf.int = TRUE)
  )
  
  saveRDS(
    list(
      'Patients' = int_1_patients,
      'Resources' = int_1_resources,
      "tests" = int_1_tests
    ),
    file = file.path(results_path, 'data_and_results.rds')
  )
  

} else if(args == 'run_int_2') { 
  # Intervention 2: Concurrent Transfer Referrals ---------------------------

 results_path <- file.path(results_path,'Intervention_2')
  
  if(!dir.exists(results_path)){
    dir.create(results_path)
  }

  int_2_raw <-
    sim_func(
        sort_by_prob = F,
        concurrent_requests = int_2and3_concurrent,
        test_concurrent = T,
        resources = T
      )
  
  int_2_patients <-
    rbindlist(lapply(
      X = int_2_raw,
      FUN = function(i)
        i[[1]]
    ), fill =  T)
  
  int_2_resources <- rbindlist(lapply(
    X = int_2_raw,
    FUN = function(i)
      i[[2]]
  ), fill = T)
  
  saveRDS(object = list(int_2_patients,int_2_resources),file = file.path(results_path,'data_and_results.rds'))
  
  int_2_coord <-
    rbindlist(
      l = mclapply(
        X = split(x = int_2_patients,
                  by = 'n.concurrent'),
        FUN = 
          function(df) {
            n.par <- unique(df[, n.concurrent])
            df <- extract_results(
              df = df,
              metric = 'total_wait_time',
              use_type = T,
              separate_vulnerable = T
            )
            return(df[, concurrent_referrals := n.par])
          },
        mc.cores = int_2and3_concurrent
      ))[!is.na(`Simulation Confidence Interval`)]
  

  int_2_results = lapply(X = list(mean,median),
                        FUN = function(fn){
                                          test_data = format_sim_patient_data(rbind(baseline_test_data,int_2_patients[n.concurrent != 1],fill=T),FUN = fn)
                                          test_data_means = test_data[,lapply(.SD,mean),.SDcols = c('TTP','total_wait_time','Travel.Distance'),list(n.concurrent,type,vulnerable_patient)]
                                          tests_df = rbind(
                                              test_data[,c(metric = 'TTP', as.list(kruskal.test(TTP ~ n.concurrent)))],
                                              test_data[,c(metric = 'coordination',as.list(kruskal.test(total_wait_time ~ n.concurrent)))],
                                              test_data[type == 'Transfer',c(metric = 'Travel.Distance',as.list((kruskal.test(Travel.Distance ~ n.concurrent))))],
                                              fill = TRUE)
                                          coord_time_pairwise = test_data[,as.list(pairwise.wilcox.test(total_wait_time,n.concurrent,p.adjust.method = 'BH'))]
                                          TTP_pairwise = test_data[,as.list(pairwise.wilcox.test(TTP,n.concurrent,p.adjust.method = 'BH'))]
                                          distance_pairwise = test_data[type == 'Transfer',as.list(pairwise.wilcox.test(Travel.Distance,n.concurrent,p.adjust.method = 'BH'))]

                                          return(list(data=test_data,
                                                      means_df = test_data_means,
                                                      tests = tests_df, 
                                                      coord_time_pairwise = coord_time_pairwise, 
                                                      TTP_pairwise = TTP_pairwise, 
                                                      distance_pairwise = distance_pairwise))
                                            })
    
  # Boxplot of averages of concurrent arrivals
  int_2_boxplots <-
    ggplot(data = melt(int_2_results[[1]]$data[TTP > 0 & type == 'Transfer' & vulnerable_patient == T,list('Avg. Total \n Coordination Time (hrs.)' = total_wait_time,"Avg. Treatment \n Delay (.hrs)" = TTP), by = n.concurrent][,n.concurrent := as.factor(n.concurrent)],id.vars = 'n.concurrent'),
           mapping = aes(y = value,
                         x =  fct_relevel(n.concurrent,c('Baseline', as.character(2:int_2and3_concurrent))) ,
                         fill = variable)) +
    geom_boxplot() + 
    geom_smooth(mapping =  aes(y = value,
                         x =  fct_relevel(n.concurrent,c('Baseline', as.character(2:int_2and3_concurrent))) ,
                         linetype = variable)) +
    xlab('Number of Concurrent Referrals') + 
    ylab("Time (hrs.)") + 
    labs(fill = 'Metric')
    
    ggsave(
      #filename = file.path(results_path, 'boxplot.jpeg'),
      filename = "Manuscript/Sections/interventions/intervention_2_boxplot.jpeg",
      plot = int_2_boxplots,
      width = 7,
      height = 4,
      device = 'jpeg',
      dpi = 700
    )

  
  saveRDS(
    list(
      int_2_patients,
      int_2_resources,
      int_2_tests),
    file = file.path(results_path, 'data_and_results.rds')
  )  
} else if(args == 'run_int_3') {

  # Intervention 3: Combination  --------------------------------------------
  results_path <- file.path(results_path,'Intervention_3')
  
  if(!dir.exists(results_path)){
    dir.create(results_path)
  }

  int_3_raw <-
    sim_func(
      sort_by_prob = T,
      concurrent_requests = int_2and3_concurrent,
      test_concurrent = T,
      resources = T
    )
  
  int_3_patients <-
    rbindlist(lapply(
      X = int_3_raw,
      FUN = function(l)
        l[[1]]
    )
    , fill = T)
  
  int_3_resources <-
    rbindlist(lapply(
      X = int_3_raw,
      FUN = function(l)
        l[[2]]
    )
    , fill = T)
  
  saveRDS(object = list(int_3_patients,int_3_resources),file = file.path(results_path,'data_and_results.rds'))
  
  baseline_test_data <- readRDS(file = file.path(results_path,'baseline_test_data.rds'))

  int_3_results = lapply(X = list(mean,median),
                        FUN = function(fn){
                                          test_data = rbind(baseline_test_data,format_sim_patient_data(int_3_patients,FUN = fn),fill=T)
                                          test_data_means = test_data[,lapply(.SD,mean),.SDcols = c('TTP','total_wait_time','Travel.Distance'),list(n.concurrent,type,vulnerable_patient)]
                                          tests_df = rbind(
                                              test_data[,c(metric = 'TTP', as.list(kruskal.test(TTP ~ n.concurrent)))],
                                              test_data[,c(metric = 'coordination',as.list(kruskal.test(total_wait_time ~ n.concurrent)))],
                                              test_data[type == 'Transfer',c(metric = 'Travel.Distance',as.list((kruskal.test(Travel.Distance ~ n.concurrent))))],
                                              fill = TRUE)
                                          coord_time_pairwise = test_data[,as.list(pairwise.wilcox.test(total_wait_time,n.concurrent,p.adjust.method = 'BH'))]
                                          TTP_pairwise = test_data[,as.list(pairwise.wilcox.test(TTP,n.concurrent,p.adjust.method = 'BH'))]
                                          distance_pairwise = test_data[type == 'Transfer',as.list(pairwise.wilcox.test(Travel.Distance,n.concurrent,p.adjust.method = 'BH'))]

                                          return(list(data=test_data,
                                                      means_df = test_data_means,
                                                      tests = tests_df, 
                                                      coord_time_pairwise = coord_time_pairwise, 
                                                      TTP_pairwise = TTP_pairwise, 
                                                      distance_pairwise = distance_pairwise))
                                            })


  # Boxplot of averages of concurrent arrivals
  int_3_boxplots <-
    ggplot(data = melt(int_3_results[[1]]$data[TTP > 0 & type == 'Transfer' & vulnerable_patient == T,list('Avg. Total \n Coordination Time (hrs.)' = total_wait_time,
                                              "Avg. Treatment \n Delay (.hrs)" = TTP),
                                              by = n.concurrent
                                              ][,n.concurrent := as.factor(n.concurrent)],id.vars = 'n.concurrent'),
           mapping = aes(y = value,
                         x =  fct_relevel(n.concurrent,c('Baseline', as.character(1:int_2and3_concurrent))) ,
                         fill = variable)) +
    geom_boxplot() + 
    geom_smooth(mapping =  aes(y = value,
                         x =  fct_relevel(n.concurrent,c('Baseline', as.character(1:int_2and3_concurrent))) ,
                         linetype = variable)) +
    xlab('Number of Concurrent Referrals') + 
    ylab("Time (hrs.)") + 
    labs(fill = 'Metric')
    
  
  ggsave(
    #filename = file.path(results_path, 'boxplot.jpeg'),
    filename="Manuscript/Sections/interventions/intervention_3_boxplot.jpeg",
    plot = int_3_boxplots,
    width = 7,
    height = 4,
    device = 'jpeg',
    dpi = 700
  )
  
  saveRDS(
    list(int_3_patients,
         int_3_resources,
         int_3_results),
    file = file.path(results_path, 'data_and_results.rds')
  )
  # rm(list = c('int_3_raw', 'int_3_TTP'))
}