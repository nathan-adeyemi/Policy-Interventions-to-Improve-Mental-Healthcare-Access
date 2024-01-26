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

# # Test Experiment Simulation Parameters (replications, warmup, etc) 
# numIters <- 3
# warm_period <- 1
# sim_period <- 5
# results_path <-
#   file.path(
#     '.',
#     'Deprecated Results',
#     'testing',
#     ifelse(test = args == 'run_baseline',yes = 'Baseline Results',no = 'Intervention Results')
#   )

int_2and3_concurrent <- 8
sim_func <- function(sort_by_prob, 
                     n.parallel,
                     resources = F,
                     test_concurrent = F) {
  results <- full_sim(
    num_iter = numIters,
    parallel = TRUE,
    prob_sort = sort_by_prob,
    warmup = warm_period,
    concurrent_requests = n.parallel,
    sim_length = sim_period,
    save_files = F,
    return_resources = resources,
    rep_parallel_combo = test_concurrent)
  return(results)
}


if(!dir.exists(results_path)){
  dir.create(results_path)
}

# Baseline Results --------------------------------------------------------
if(args == 'run_baseline'){
  
  res <-
    sim_func(sort_by_prob = F,
             n.parallel = 1,
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
      df = baseline_patients[type == 'Transfer'][, `:=`(in_range = Travel.Distance <= 25)],
      metric = 'in_range',
      use_type = F,
      separate_vulnerable = T
    ),
    transfer_within_50 = extract_results(
      df = baseline_patients[type == 'Transfer'
                             ][, `:=`(in_range = Travel.Distance <= 50)],
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
  baseline_test_data <- baseline_patients[,vulnerable_patient := (Age != 'Adult'),
                                   by = list(Sim.ID, replication)
                                   ][vulnerable_patient == T & type == 'Transfer'
                                     ][, `:=`(TTP = sum(total_wait_time, Travel.time)), by = list(Sim.ID, replication)
                                       ][, .(TTP = mean(x = TTP, na.rm = T), 
                                             total_wait_time = mean(x = total_wait_time,na.rm = T)),
                                         by = list(replication)][,n.concurrent := 'baseline']
  saveRDS(
    list(baseline_patients, baseline_resources,baseline_results),
    file = file.path(results_path, 'baseline_data_and_results.rds')
  )
  
  saveRDS(baseline_test_data,file = file.path(results_path,'baseline_test_data.rds'))
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
             resources = T)Lmaso
  
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
  
  int_1_TTP_test_data <-
    rbind(int_1_patients[, `:=`(df = 'intervention_1')], 
          baseline_patients[, `:=`(df = 'baseline')], 
          fill = T)[, .(TTP = one.boot(data = TTP,
                                           FUN = mean,
                                           R = 500,na.rm = T)[['t0']],
                        total_wait_time =one.boot(data = total_wait_time,
                                                  FUN = mean,
                                                  R = 500,na.rm = T)[['t0']]),
                        by = list(type,`Vulnerable Patient`,replication,df)]
  
  int_1_TTP_test <-
    t.test(formula = TTP ~ df, 
           data = int_1_TTP_test_data[`Vulnerable Patient` == T & type == 'Transfer'])
  
  int_1_TTP_test_np <-
    wilcox.test(
      TTP ~ df,
      data = int_1_TTP_test_data[`Vulnerable Patient` == T &
                                   type == 'Transfer'],
      na.rm = TRUE,
      paired = FALSE,
      exact = FALSE,
      conf.int = TRUE
    )
  
  int_1_total_wait_time_test <-
    t.test(formula = total_wait_time ~ df, 
           data = int_1_TTP_test_data[`Vulnerable Patient` == T & type == 'Transfer'])
  
  int_1_total_wait_time_test_np <-
    wilcox.test(
      total_wait_time ~ df,
      data = int_1_TTP_test_data[`Vulnerable Patient` == T &
                                   type == 'Transfer'],
      na.rm = TRUE,
      paired = FALSE,
      exact = FALSE,
      conf.int = TRUE
    )
  
  
  int_1_TTP_test_nonVuln <- t.test(x = int_1_TTP_test_data[`Vulnerable Patient` == F & 
                                                             type == 'Transfer' & 
                                                             df == 'intervention_1',TTP],
                                   y = int_1_TTP_test_data[`Vulnerable Patient` == F & 
                                                             type == 'Transfer' & 
                                                             df == 'baseline',TTP])
  int_1_TTP_test_internal <- t.test(x = int_1_TTP_test_data[type == 'Internal' & 
                                                              df == 'intervention_1',TTP],
                                    y = int_1_TTP_test_data[type == 'Internal' & 
                                                              df == 'baseline',TTP])
  
  saveRDS(
    list(
      'Patients' = int_1_patients,
      'Resources' = int_1_resources,
      "Treatment Delay Test" = int_1_TTP_test,
      "Coordination Time Test" = int_1_total_wait_time_test_np,
      "Adult Treatment Delay Test" = int_1_TTP_test_nonVuln,
      "Internally Placed Test" = int_1_TTP_test_internal
      
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
        n.parallel = int_2and3_concurrent,
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
  

  test_data_2 <- rbind(baseline_test_data,
                     int_2_patients[,vulnerable_patient := (Age != 'Adult'),
                         by = list(Sim.ID, replication,n.concurrent)
                         ][vulnerable_patient == T& type == 'Transfer'
                           ][, `:=`(TTP = sum(total_wait_time, Travel.time)), by = list(Sim.ID, replication,n.concurrent)
                             ][, .(TTP = mean(x = TTP, na.rm = T), 
                                   total_wait_time = mean(x = total_wait_time,na.rm = T)),
                               by = list(replication,n.concurrent)],fill = T)
  
  int_2_tests <- list(
    int_2_testDF =
      rbind(
        test_data_2[,c(metrix = 'TTP', as.list(kruskal.test(TTP ~ n.concurrent)))],
        test_data_2[,c(metric = 'coordination',as.list(kruskal.test(total_wait_time ~ n.concurrent)))],
        fill = TRUE),
    coord_time_pairwise = test_data_2[,as.list(pairwise.wilcox.test(total_wait_time,n.concurrent,p.adjust.method = 'BH'))],
    TTP_pairwise = test_data_2[,as.list(pairwise.wilcox.test(TTP,n.concurrent,p.adjust.method = 'BH'))])
  
  # Boxplot of averages of concurrent arrivals
  int_2_boxplots <-
    ggplot(data = test_data_2[,`:=`(n.concurrent = as.factor(n.concurrent),
                                    `Transfer Coordination Time (hrs.)` = total_wait_time)
                              ][n.concurrent != 1],
           mapping = aes(y = `Transfer Coordination Time (hrs.)`,
                         x =  fct_relevel(n.concurrent,c('baseline', as.character(2:int_2and3_concurrent))) ,
                         fill = fct_relevel(n.concurrent,c('baseline', as.character(2:int_2and3_concurrent))))) +
    geom_boxplot() + 
    geom_smooth() +
    theme(legend.title = element_blank(),
          legend.position = 'none') + 
    xlab('Number of Concurrent Referrals') 
    
    ggsave(
      filename = file.path(results_path, 'boxplot.jpeg'),
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
      int_2_coord,
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
      n.parallel = int_2and3_concurrent,
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
  
  int_3_TTP <-
    rbindlist(
      l = lapply(
        X = split(x = int_3_patients,
                  by = 'n.concurrent'),
        FUN = 
          function(df) {
            n.par <- unique(df[, n.concurrent])
            df <- extract_results(
              df = df[, `:=`(TTP = sum(total_wait_time, Travel.time)),
                       by = list(Sim.ID, replication)],
              metric = 'TTP',
              use_type = T,
              separate_vulnerable = T
            )
            return(df[, concurrent_referrals := n.par])
          }
      ))[!is.na(`Simulation Confidence Interval`)]
  
  baseline_test_data <- readRDS(file = file.path(results_path,'baseline_test_data.rds'))
  
  test_data_3 <- rbind(baseline_test_data,
                     int_3_patients[,vulnerable_patient := (Age != 'Adult'),
                         by = list(Sim.ID, replication,n.concurrent)
                         ][vulnerable_patient == T & type == 'Transfer'
                           ][, `:=`(TTP = sum(total_wait_time, Travel.time)), by = list(Sim.ID, replication,n.concurrent)
                             ][, .(TTP = mean(x = TTP, na.rm = T), 
                                   total_wait_time = mean(x = total_wait_time,na.rm = T)),
                               by = list(replication,n.concurrent)],fill = T
                     )[n.concurrent == 'baseline',n.concurrent := 'Baseline Model w/\nDistance Prioritization']

  int_3_tests <- list(
    int_3_testDF =
      rbind(
        test_data_3[,c(metrix = 'TTP', as.list(kruskal.test(TTP ~ n.concurrent)))],
        test_data_3[,c(metric = 'coordination',as.list(kruskal.test(total_wait_time ~ n.concurrent)))],
        fill = TRUE),
    coord_time_pairwise = test_data_3[,as.list(pairwise.wilcox.test(total_wait_time,n.concurrent,p.adjust.method = 'BH'))],
    TTP_pairwise = test_data_3[,as.list(pairwise.wilcox.test(TTP,n.concurrent,p.adjust.method = 'BH'))])
  
  # Boxplot of averages of concurrent arrivals
  int_3_boxplots <- 
    ggplot(data = test_data_3[,`:=`(n.concurrent = as.factor(n.concurrent),
                                                  `Time-to-Placement (hrs.)` = TTP)],
                           mapping = aes(y = `Time-to-Placement (hrs.)`,
                                         x = fct_relevel(n.concurrent,c('Baseline Model w/\nDistance Prioritization', as.character(seq(int_2and3_concurrent)))),
                                         fill = fct_relevel(n.concurrent,c('Baseline Model w/\nDistance Prioritization', as.character(seq(int_2and3_concurrent)))))) +
    geom_boxplot() + 
    theme(legend.title = element_blank(),
          legend.position = 'none') + 
    xlab('Number of Concurrent Referrals') 
  
  ggsave(
    filename = file.path(results_path, 'boxplot.jpeg'),
    plot = int_3_boxplots,
    width = 7,
    height = 4,
    device = 'jpeg',
    dpi = 700
  )
  
  saveRDS(
    list(int_3_patients,
         int_3_resources,
         int_3_TTP,
         int_3_tests),
    file = file.path(results_path, 'data_and_results.rds')
  )
  # rm(list = c('int_3_raw', 'int_3_TTP'))
}