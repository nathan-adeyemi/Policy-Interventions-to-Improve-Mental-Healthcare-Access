source("functions.R")
source('Minnesota MH Network Simulation.R')
siteInfo <-
  data.table(readRDS(file.path(
    ".",
    "Data",
    "Function Requirements",
    "Rates5.rds"
  )))

# Run Simulation for many reps with a long sim length ---------------------
# full_case <- readline(prompt = "Run experiments for full run length and all simulation replications?") 
# run_baseline <- readline(prompt = "Run for baseline results:") 
# run_int_1 <- readline(prompt = "Run for Intervention 1 results:") 
# run_int_2 <- readline(prompt = "Run for Intervention 2 results:") 
# run_int_3 <- readline(prompt = "Run for Intervention 3 results:") 

run_baseline = run_int_1 = run_int_2 = F
full_case = run_int_3 = T

sapply(
  X = c(
    'full_case',
    'run_baseline',
    'run_int_1',
    'run_int_2',
    'run_int_3'
  ),
  FUN = function(i)
    assign(x = i, 
           value = as.logical(
             get(x = i)))
)

if(full_case) {
  # Full Experiment Simulation Parameters (replications, warmup, etc) 
  numIters <- 20
  warm_period <- 30
  sim_period <- 200
} else {
  # Test Experiment Simulation Parameters (replications, warmup, etc) 
  numIters <- 10
  warm_period <- 0
  sim_period <- 1
}

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
results_path <-
  file.path(
    '.',
    'Simulation and Alternatives',
    'Intervention Results'
  )
if(!dir.exists(results_path)){
  dir.create(results_path)
}

# Baseline Results --------------------------------------------------------
if(run_baseline){
  baseline_patients <-
    sim_func(sort_by_prob = F,
             n.parallel = 1,
             resources = T)
  baseline_resources <- baseline_patients[[2]]
  baseline_patients <- baseline_patients[[1]]
  
  baseline_results = list(
    average_TTP = extract_results(
      df = baseline_patients[, `:=`(TTP = sum(total_wait_time, 
                                              Travel.time)), 
                             by = Sim.ID],
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
  baseline_test_data <- baseline_1[,vulnerable_patient := (Age != 'Adult'),
                                   by = list(Sim.ID, replication)
                                   ][vulnerable_patient == T & type == 'Transfer'
                                     ][, `:=`(TTP = sum(total_wait_time, Travel.time)), by = list(Sim.ID, replication)
                                       ][, .(TTP = mean(x = TTP, na.rm = T), 
                                             total_wait_time = mean(x = total_wait_time,na.rm = T)),
                                         by = list(replication)][,n.parallel := 'baseline']
  saveRDS(
    list(baseline_patients, baseline_resources,baseline_results),
    file = file.path(results_path, 'baseline_data_and_results.rds')
  )
  openxlsx::write.xlsx(baseline_results,
                       file = file.path(results_path, 'Baseline_Processed_Results.xlsx'))
  rm(list = c('baseline_resources'))
}

# Intervention 1: Patients routed by Pr{Facility Acceptance} --------------
if(run_int_1) {
  int_1_patients <-
    sim_func(sort_by_prob = T,
             n.parallel = 1,
             resources = F)
  
  int_1_patients <-
    int_1_patients[, `:=`(TTP = sum(total_wait_time, Travel.time)), by = Sim.ID]
  
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
      'Raw Data' = int_1_patients,
      "Treatment Delay Test" = int_1_TTP_test,
      "Coordination Time Test" = int_1_total_wait_time_test_np,
      "Adult Treatment Delay Test" = int_1_TTP_test_nonVuln,
      "Internally Placed Test" = int_1_TTP_test_internal
    ),
    file = file.path(results_path, 'intervention_1_data_and_results.rds')
  )
  
  rm(
    list = c(
      'baseline_patients',
      'int_1_patients',
      'intervention_1_TTP',
      'int_1_TTP_test',
      'int_1_TTP_test_nonVuln',
      'int_1_TTP_test_internal',
      'int_1_TTP_test_data',
      'int_1_TTP_Test_np',
      'int_1_total_wait_time_test',
      'int_1_total_wait_time_test_np'
    )
  )
}

# Intervention 2: Concurrent Transfer Referrals ---------------------------

if(run_int_2) {
  
  int_2_raw <-
    rbindlist(
      l = sim_func(
        sort_by_prob = F,
        n.parallel = int_2and3_concurrent,
        test_concurrent = T
      ),
      fill = T
    )
  
  saveRDS(int_2_raw,
          file = file.path(results_path, 'intervention_2_data_and_results.rds'))
  
  int_2_coord <-
    rbindlist(
      l = mclapply(
        X = split(x = int_2_raw,
                  by = 'n.parallel'),
        FUN = 
          function(df) {
            n.par <- unique(df[, n.parallel])
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
                     int_2_raw[,vulnerable_patient := (Age != 'Adult'),
                         by = list(Sim.ID, replication,n.parallel)
                         ][vulnerable_patient == T& type == 'Transfer'
                           ][, `:=`(TTP = sum(total_wait_time, Travel.time)), by = list(Sim.ID, replication,n.parallel)
                             ][, .(TTP = mean(x = TTP, na.rm = T), 
                                   total_wait_time = mean(x = total_wait_time,na.rm = T)),
                               by = list(replication,n.parallel)],fill = T)
  
  int_2_tests <- list(
    TTP_test =
      kruskal.test(formula = TTP ~ n.parallel,
                   data = test_data),
    
    coord_time_test =
      kruskal.test(formula = total_wait_time ~ n.parallel,
                   data = test_data),
    
    
    coord_time_pairwise =
      tukeyTest(total_wait_time ~ n.parallel ,
                data = test_data, dist = 'KruskalWallis'),
    
    TTP_pairwise =
      tukeyTest(TTP ~ n.parallel ,
                data = test_data, dist = 'KruskalWallis')
  )
  
  # Boxplot of averages of concurrent arrivals
  int_2_boxplots <-
    ggplot(data = test_data_2[,`:=`(n.parallel = as.factor(n.parallel),
                                    `Transfer Coordination Time (hrs.)` = total_wait_time)
                              ][n.parallel != 1],
           mapping = aes(x = `Transfer Coordination Time (hrs.)`,
                         group =  fct_relevel(n.parallel,c('baseline', as.character(2:8))) ,
                         fill = fct_relevel(n.parallel,c('baseline', as.character(2:8))))) +
    geom_boxplot() + 
    guides(fill = guide_legend(title = "Number of\nConcurrrent\nReferrals")) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) + 
    scale_fill_grey() + coord_flip()
    
  
    ggsave(
      filename = file.path(results_path, 'intervention_2_boxplot.jpeg'),
      plot = int_2_boxplots,
      width = 7,
      height = 3,
      device = 'jpeg',
      dpi = 700
    )
  
  saveRDS(
    list(
      int_2_raw,
      int_2_coord,
      concurrent_referrals_tests
    ),
    file = file.path(results_path, 'intervention_2_data_and_results.rds')
  )
  rm(list = c(
    'int_2_raw',
    'concurrent_referral_TTP',
    'test_data'
  ))
}

# Intervention 3: Combination  --------------------------------------------
if(run_int_3) {
  int_3_raw <-
    sim_func(
      sort_by_prob = T,
      n.parallel = int_2and3_concurrent,
      test_concurrent = T
    )
  
  int_3_raw <-
    rbindlist(l = int_3_raw,
              fill = T)
  
  saveRDS(int_3_raw, file = file.path(results_path, 'intervention_3_data_and_results.rds'))
  
  int_3_TTP <-
    rbindlist(
      l = lapply(
        X = split(x = int_3_raw,
                  by = 'n.parallel'),
        FUN = 
          function(df) {
            n.par <- unique(df[, n.parallel])
            browser()
            df <- extract_results(
              df = df[ `:=`(TTP = sum(total_wait_time, Travel.time)),
                       by = list(Sim.ID, replication)],
              metric = 'TTP',
              use_type = T,
              separate_vulnerable = T
            )
            return(df[, concurrent_referrals := n.par])
          }
      ))[!is.na(`Simulation Confidence Interval`)]
  
  test_data_3 <- rbind(baseline_test_data,
                     int_3_raw[,vulnerable_patient := (Age != 'Adult'),
                         by = list(Sim.ID, replication,n.parallel)
                         ][vulnerable_patient == T & type == 'Transfer'
                           ][, `:=`(TTP = sum(total_wait_time, Travel.time)), by = list(Sim.ID, replication,n.parallel)
                             ][, .(TTP = mean(x = TTP, na.rm = T), 
                                   total_wait_time = mean(x = total_wait_time,na.rm = T)),
                               by = list(replication,n.parallel)],fill = T)
  
  int_3_tests <- list(
    TTP_test =
      kruskal.test(formula = TTP ~ n.parallel,
                   data = test_data),
    
    coord_time_test =
      kruskal.test(formula = total_wait_time ~ n.parallel,
                   data = test_data),
    
    
    coord_time_pairwise =
      tukeyTest(total_wait_time ~ n.parallel ,
                data = test_data, dist = 'KruskalWallis'),
    
    TTP_pairwise =
      tukeyTest(TTP ~ n.parallel ,
                data = test_data, dist = 'KruskalWallis')
  )
  
  # Boxplot of averages of concurrent arrivals
  int_3_boxplots <- ggplot(data = test_data_3[,`:=`(n.parallel = as.factor(n.parallel),
                                                  `Time-to-Placement (hrs.)` = TTP)],
                           mapping = aes(x = `Time-to-Placement (hrs.)`,
                                         group = fct_relevel(n.parallel,c('baseline', as.character(seq(8)))),
                                         fill = fct_relevel(n.parallel,c('baseline', as.character(seq(8)))))) +
    geom_boxplot() + 
    guides(fill = guide_legend(title = "Number of\nConcurrrent\nReferrals")) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_fill_grey() + coord_flip()
  
  
  
  ggsave(
    filename = file.path(results_path, 'intervention_3_boxplot.jpeg'),
    plot = int_3_boxplots,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  saveRDS(
    list(int_3_raw,
         int_3_TTP,
         int_3_tests),
    file = file.path(results_path, 'intervention_3_data_and_results.rds')
  )
  # rm(list = c('int_3_raw', 'int_3_TTP'))
}