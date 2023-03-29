boot_confInt_target_val <- function(x, FUNCTION = {function(i) mean(i,na.rm = T)}, interval = 95,interval_type = 'basic'){
  boot_name <- switch(interval_type,
                      'basic' = 'basic',
                      'perc' = 'percent',
                      'norm' = 'normal',
                      'stud' = 'student',
                      'bca' = 'bca')
  p <- boot.ci(one.boot(x,FUNCTION,500),conf = interval * 0.01,type = interval_type)[[boot_name]]
  endpoint_indices <- ifelse(rep(interval_type == 'norm',2), c(2,3),c(4,5))
  
  p <- p[endpoint_indices] %>% {function(x) paste0('(',x[1],",",x[2],")")}()
  return(p)
}