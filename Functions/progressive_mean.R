progressive_mean <- function(df){ 
  sapply(
    X = seq(nrow((df))),
    FUN = function(i)
      mean(df[seq(i), 'wait_times'] %>% unlist(), na.rm = T)
  )
}