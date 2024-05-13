rep_prog_func <- function(x) {
    data.table(Ts = as.double(x$IP.Arrival.Timestamp),
               Average.TTP = progressive_mean(x))
  }