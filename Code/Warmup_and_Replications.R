results_path <- file.path("Data",'Warmup and Replication Analysis Results')
source(file.path('Simulations','Minnesota MH Network Simulation.R'))
rep_results <- full_sim(num_iter = availableCores() - 1,
                        warmup = 0,
                        sim_length = 600,
                        save_files = F,
                        return_resources = T)

saveRDS(rep_results,file.path(results_path,'raw_results.rds'))
patients <-
  rbindlist(mclapply(
    X  = rep_results,
    FUN = function(dt){
      dt$patients
    },
    mc.cores = availableCores()-1
  ))
resources <-
  rbindlist(mclapply(
    X  = rep_results,
    FUN = function(dt){
      dt$resources
    },
    mc.cores = availableCores() -1
  ))
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
  ))[until <= 200,]

ttp_transfers <- rbindlist(lapply(
  X = with(patients, min(day_no):max(day_no)),
  FUN = function(i)
    patients[day_no <= i & type == 'Transfer'
    ][, .(until = i, ttp = mean(total_wait_time, na.rm = T)), 
      by = list(vulnerable, replication)]
))[until <= 200,]

p1 <-
  ggplot(ttp_means[vulnerable==T & replication <= 25],
         aes(until, ttp)) + 
  geom_line(aes(group = replication),
            alpha = 0.1) +
  stat_summary(geom = 'line',
               fun.y = mean,
               color = 'blue') + 
  xlab('Average Value Up to Day #:') +
  ylab('Treatment Delay (hrs.)')
  
p2 <-
  ggplot(ttp_means[vulnerable==F & replication <= 25],
         aes(until, ttp)) + 
  geom_line(aes(group = replication),
            alpha = 0.1) +
  stat_summary(geom = 'line',
               fun.y = mean,
               color = 'red') + 
  xlab('Average Value Up to Day #:') +
  ylab('Treatment Delay (hrs.)')

p3 <- 
  ggplot(ttp_transfers[vulnerable==T & replication <= 25],
         aes(until, ttp)) + 
  geom_line(aes(group = replication),
            alpha = 0.1) +
  stat_summary(geom = 'line',
               fun.y = mean,
               color = '#0F9B13') + 
  xlab('Average Value Up to Day #:') +
  ylab('Treatment Delay (hrs.)')

  ggsave(
    filename = file.path(results_path, 'Treatment_Delay_Convergence_VP.jpeg'),
    plot = p1,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  ggsave(
    filename = file.path(results_path, 'Treatment_Delay_Convergence_NVP.jpeg'),
    plot = p2,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  
  ggsave(
    filename = file.path(results_path, 'Treatment_Delay_Convergence_VP_Transfers.jpeg'),
    plot = p3,
    width = 7,
    height = 3,
    device = 'jpeg',
    dpi = 700
  )
  

saveRDS(list('patients' = patients,
             'resources' = resources,
             'ttp_means' = ttp_means),
        file = file.path(results_path,'convergence_results.rds'))
