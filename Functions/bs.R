bs <- function(x) {
  FUNCTION = val_func
  interval = 95
  interval_type = 'basic'
  # boot_confInt_target_val
  boot_name <- switch(
    interval_type,
    'basic' = 'basic',
    'perc' = 'percent',
    'norm' = 'normal',
    'stud' = 'student',
    'bca' = 'bca'
  )
  endpoint_indices <-
    ifelse(rep(interval_type == 'norm', 2), c(2, 3), c(4, 5))
  
  if (length(unique(x)) >= 10) {
    x = remove_outliers(x)
  }
  
  p <- tryCatch({
    signif(boot.ci(
      one.boot(x, FUNCTION, 500),
      conf = interval * 0.01,
      type = interval_type
    )[[boot_name]][endpoint_indices],
    4) %>%
      {function(x) paste0('(', x[1], ",", x[2], ")")}()
  },
  error = function(e) {
    err <- cat("ERROR :", conditionMessage(e))
    return (err)
  })
  
  return(p)
}