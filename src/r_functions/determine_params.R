determine_params <- function(patients){
  
  # Number of Replications Analysis Plot
  ages = c('Adolescent','Adult','Child','Geriatric')
  numIters <- max(patients$replication)
  
  # Plots average wait time against simulation time to find warmup
  listpatients <- patients %>% split(.,patients$replication)
  
  if(Sys.info()['sysname'] != 'Darwin'){
    cl = makeCluster(detectCores(logical = T)-1,outfile = "")
    registerDoParallel(cl)
    repProgMeans <- foreach(i = listpatients,
                            .export = c('listpatients','progressive_mean'),
                            .packages = c('data.table','dplyr')) %dopar% rep_prog_func(i)
    stopImplicitCluster()
  } else {
    repProgMeans <- mclapply(listpatients,FUN = function(i) rep_prog_func(i),mc.cores = detectCores() - 2)
  }
  
  # Plots output statistic mean with respect to time
  repProgMeansDf <- rbindlist(repProgMeans,idcol = 'Rep')
  ggplot(repProgMeansDf,aes(x = Ts, y = Average.TTP)) +
    geom_line(color = 'blue', alpha = .1, aes(group = Rep)) + geom_smooth(se = TRUE, color = 'black',size = 0.6)
  
  # Plots of average wait time by number of replications
  plot_data <- sapply(ages, function(age) sapply(seq(max(numIters)),
                                                 function(i) mean(replicate(10, mean(patients[replication %in% sample(unique(patients$replication),size = i) &
                                                                                                Age ==age, wait_times],na.rm = T))))) %>%
    data.table() %>% mutate(Replication = seq(numIters)) %>% pivot_longer(!Replication)
  
  overall_plot <- sapply(seq(max(numIters)),function(i) mean(replicate(10, mean(patients[replication %in% sample(unique(patients$replication),size = i), wait_times],na.rm = T)))) %>%
    data.table() %>% mutate(Replication = seq(numIters)) %>% pivot_longer(!Replication)
  
  ggplot(plot_data,aes(x = Replication, y = value, color = name)) + geom_line()
  ggplot(overall_plot, aes(x = Replication, y = value)) + geom_line()
}
