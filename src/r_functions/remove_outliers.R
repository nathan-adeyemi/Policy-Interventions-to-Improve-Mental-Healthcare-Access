remove_outliers <- function(x, cutoff = 0.025) {
  ub <- quantile(x, 1 - cutoff, na.rm = T)
  lb <- quantile(x, cutoff, na.rm = T)
  return(x[x > lb & x < ub])
}