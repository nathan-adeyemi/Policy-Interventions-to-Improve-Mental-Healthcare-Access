time_extract <- function(t1,t2,t3){
  t2 <- min(c(t2,t3)[!is.infinite(as.numeric(c(t2,t3)))])
  if (t1 == t2){
    return(NA_real_)
  }else{
    return(abs(as.numeric(difftime(t1,t2,units = 'hours'))))
  }
}