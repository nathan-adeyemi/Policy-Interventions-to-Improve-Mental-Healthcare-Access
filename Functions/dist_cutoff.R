dist_cutoff <- function(max_quant,distribution){
  data <- inpatient_stays[inpatient_stays < quantile(inpatient_stays,max_quant)]
  data <- sample(data,3000,replace = F)
  return(fitdist(data,distr = distribution)$loglik)
}