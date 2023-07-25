results_path <- file.path("Data",'Warmup and Replication Analysis Results')
source(file.path('Simulations','Minnesota MH Network Simulation.R'))
rep_results <- full_sim(num_iter = availableCores() - 1,
                        warmup = 0,
                        sim_length = 600,
                        save_files = F,
                        return_resources = T)

saveRDS(rep_results,file.path(results_path,'raw_results.rds'))
patients <- rep_results[[1]]
resources <- rep_results[[2]]
patients <- patients[,`:=`(day_no = simtimer::sim_date(Enter.Timestamp * 3600),
                           vulnerable = Age != 'Adult')]
setkey(patients,vulnerable)

ttp_means <-
  rbindlist(lapply(
    X = with(patients, min(day_no):max(day_no)),
    FUN = function(i)
      patients[day_no <= i
               ][, .(until = i, ttp = mean(total_wait_time, na.rm = T)), 
                   by = list(vulnerable, replication)]
  ))
p1 <- ggplot(ttp_means[vulnerable == T],aes(until,ttp)) + geom_line(aes(group = replication),alpha = 0.05) + stat_summary(geom = 'line',fun.y = mean,color = 'red')
p2 <- ggplot(ttp_means[vulnerable == F],aes(until,ttp)) + geom_line(aes(group = replication),alpha = 0.05) + stat_summary(geom = 'line',fun.y = mean,color = 'blue')
convergence_plots <- gridExtra::grid.arrange(p1,p2,ncol = 1)

  ggsave(
    filename = file.path(sa_path, 'convergence_plots.jpeg'),
    plot = convergence_plots,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )

saveRDS(list('patients' = patients,
             'resources' = resources,
             'ttp_means' = ttp_means),
        file = file.path(results_path,'convergence_results.rds'))
