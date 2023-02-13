if (Sys.info()['user'] == 'adeyemi.n'){
  prefix = 'C:/Users/adeyemi.n'
}else if(Sys.info()['sysname'] == 'Darwin'){
  prefix = '/Users/nadeyemi'
}else{
  prefix = 'Y:'
}
invisible(lapply(c('data.table','readxl','plyr','tidyverse','boot','simpleboot',
                    'ggplot2','gtools','tictoc','compiler','ggpubr'), library, character.only = TRUE))

setwd(paste0(prefix,'/OneDrive - Northeastern University/Graduate/Research/Mental Health'))

# Read other files and functions in
debugSource('./Code/Simulation Project/Simulation and Alternatives/functions.R') # Source files with all other custom functions
siteInfo <- readRDS('./Data/Function Requirements/Rates3.rds')

full_sim()

# Tests allowing social workers to send multiple transfer requests concurrently
tic()
nParallelResults <- lapply(seq(8), function(i) full_sim( n.parallel = i))
toc()

for (i in seq_along(nParallelResults)){
  nParallelResults[[i]] <- nParallelResults[[i]] %>% mutate(N.Parallel = i)
}
nParallelResultsDF <- rbindlist(nParallelResults,use.names = TRUE) 
save(nParallelResultsDF,file = './Code/Simulation Project/Simulation and Alternatives/Concurrent Referrals Results.rdata')

# Tests letting the SW randomly chooose a transfer site within N miles
tic()
nCheckResults <- lapply(seq(8), function(i) full_sim(n_check = i))
toc()

for (i in seq_along(nCheckResults)){
  nCheckResults[[i]] %<>% mutate(N.Check = i)
}
nCheckResultsDF <- rbindlist(nCheckResults,use.names = TRUE) 
saveRDS(nCheckResultsDF,file = './Code/Simulation Project/Simulation and Alternatives/Random Referral in top N Facilities.rds')

meansDF <- nParallelResultsDF %>% group_by(N.Parallel) %>% 
  summarise(Mean_Wait = 
              boot_confInt(wait_times,function(x) mean(x,na.rm = T)),
            Standard.Dev =  boot_confInt(wait_times,function(x) sd(x,na.rm = T)))

meansTest <- oneway.test(wait_times ~ N.Parallel,nParallelResultsDF,na.action = na.omit)

pairwiseTests <- PMCMRplus::tukeyTest(wait_times ~ N.Parallel,
                                      data = nParallelResultsDF, dist = 'KruskalWallis')

multi_parallel_Boxplots <- results_boxplot(nParallelResultsDF,N.Parallel)

multi_parallel_Density <- results_density(nParallelResultsDF)



meansDF <- nCheckResultsDF %>% group_by(N.Check) %>% 
  summarise(Mean_Wait = 
              boot_confInt(wait_times,function(x) mean(x,na.rm = T)),
            Standard.Dev =  boot_confInt(wait_times,function(x) sd(x,na.rm = T)))

meansTest <- oneway.test(wait_times ~ N.Check,nCheckResultsDF,na.action = na.omit)

pairwiseTests <- PMCMRplus::tukeyTest(wait_times ~ N.Check,
                                      data = nCheckResultsDF, dist = 'KruskalWallis')

multi_check_Boxplots <- results_boxplot(nCheckResultsDF,N.Check)

multi_check_Density <- results_density(nCheckResultsDF)
toc()
