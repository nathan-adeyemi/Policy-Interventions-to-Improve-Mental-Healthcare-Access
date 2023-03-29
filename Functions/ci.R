ci <- function(x, weights, conf.level = 0.95) {
  nx <- length(x)
  df <- nx - 1
  vx <- weighted.var(x = x, w = weights) 
  mx <- weighted.mean(x, weights)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  cint * stderr
}