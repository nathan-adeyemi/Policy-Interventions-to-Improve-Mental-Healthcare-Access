weighted_mean_ci <- function(data, weights){
  w.mean = Hmisc::wtd.mean(x = data, weights = weights)
  w.sd = sqrt(Hmisc::wtd.var(x = data, weights = weights))
  
  return(c(w.mean - 1.96 * w.sd,
           w.mean + 1.96 * w.sd))
}