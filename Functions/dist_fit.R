dist_fit <- function(data){
  data <- as.numeric(data)
  if(length(na.omit(data)) >= 10){
    dist_type <- names(which.min(mclapply(test_dists(data),function(i) i$aic)))
    fit.estimate <- summary(fitdist(data,dist_type))$estimate
    return(list(parameter_estimates <- fit.estimate))
  }else{
    return(list(mean(data)))
  }
}