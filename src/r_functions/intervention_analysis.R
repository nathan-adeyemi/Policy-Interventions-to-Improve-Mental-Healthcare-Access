intervention_1_analysis <- function(inputData) {
  intervention_data <-
    readRDS(
      "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Results/interventions/intervention_results.rds"
    )
  intervention_data <-
    intervention_data[grepl("median", variable, ignore.case = TRUE),][, concurrent_requests := as.character(concurrent_requests)][concurrent_requests == 1 &
                                                                                                                                    sort_by_prob == FALSE, concurrent_requests := "Baseline"][, concurrent_requests := factor(concurrent_requests, levels = c("Baseline", as.character(seq(1, 8))))]
  
  int_1_splits <-
    split(intervention_data[(concurrent_requests == 1 | concurrent_requests == "Baseline")],
          by = c("Vulnerable Patient", "type", "variable"))
  int_1_tests  <-  lapply(int_1_splits, function(data)
    data[, wilcox.test(
      value ~ sort_by_prob ,
      na.rm = TRUE,
      paired = FALSE,
      exact = FALSE,
      conf.int = TRUE
    )])
  
  # Plot the intervention Results
  
  plot_intervention_combs(interventions_df = intervention_data[sort_by_prob == F], intervention_num = 2)
  plot_intervention_combs(interventions_df = intervention_data[sort_by_prob == T  |
                                                                 concurrent_requests == 'Baseline'], intervention_num = 3)
}


intervention_stat_tests <-
  function(interventions_df) {
    unique_combinations <-
      unique(interventions_df[, .(`Vulnerable Patient`, type, variable, sort_by_prob)])
    
    # Loop through each combination and create the plot
    for (comb in 1:nrow(unique_combinations)) {
      vulnerable_patient <-
        (unique_combinations[comb, `Vulnerable Patient`])
      type_value <- unique_combinations[comb, type]
      metric_name <- unique_combinations[comb, variable]
      intervention_num <- `if`(unique_combinations[comb, sort_by_prob], 3, 2)
      
      test_dt <- interventions_df[(`Vulnerable Patient` == vulnerable_patient) &
                                    (type == type_value) &
                                    (sort_by_prob == unique_combinations[comb, sort_by_prob])]
      
      if(unique_combinations[comb,sort_by_prob]){
        test_dt = rbind(test_dt,
                        interventions_df[(`Vulnerable Patient` == vulnerable_patient) & (type == type_value) & (variable == metric_name) & (concurrent_requests == 'Baseline')])
      }
      groups <- with(test_dt, split(value, concurrent_requests))
      groups <- Filter(function(x) length(x) > 0, groups)
      
      # Create a matrix of pairwise differences
      n_groups <- length(groups)
      medians <- p_vals <- conf_ints <- matrix(NA_real_, nrow = n_groups, ncol = n_groups)
      for (i in 1:(n_groups - 1)) {
        for (j in (i + 1):n_groups) {
          pair_comp <- with(
            test_dt,
            wilcox.test(
              x = groups[[j]],
              y = groups[[i]],
              alternative = 'l',
              exact = F,
              conf.int = T,
              paired = F
            )
          )
          medians[j,i] <- pair_comp$estimate
          conf_ints[j,i] <- diff(pair_comp <- with(
            test_dt,
            wilcox.test(
              x = groups[[j]],
              y = groups[[i]],
              exact = F,
              conf.int = T,
              paired = F
            )
          )$conf.int)/2
        }
      }
      
      tests <- list(
        "experiment ANOVA" = aov(value ~ concurrent_requests, data = test_dt),
        "experiment ANOVA Summary" = summary(aov(value ~ concurrent_requests, data = test_dt)),
        "Pairwise Comparisons P-Values" = with(test_dt,pairwise.wilcox.test(value, concurrent_requests, p.adjust.method = 'bonf')),
        "Pairwise Comparisons Difference of Medians" = medians,
        "Pairwise Comparisons Confidence Intervals" = conf_ints
      )
      
      # browser(expr = (type_value == 'Transfer') & (vulnerable_patient == "TRUE") & (metric_name == "Median Treatment Delay"))
      # Create a unique filename for each plot
      filename <-
        paste0(
          "Results/interventions/intervention-",
          intervention_num,
          "/",
          ifelse(vulnerable_patient, "non-adult", "adult"),
          "-",
          type_value,
          "-",
          metric_name,
          "-statistical-tests.rds"
        )
      # Save the plot
      
      saveRDS(tests, filename)
    }
  }