samplewmean <- function(d, i, j) {
  d <- d[i]
  w <- j[i]
  return(weighted.mean(d, w))
}