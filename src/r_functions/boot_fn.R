boot_fn <-
  function(dataframe,
           column,
           split_list,
           use_type = T,
           stat = function(x)
             mean(x, na.rm = T),
           include_outliers = F) {
    if ((length(split_list) == 1 & use_type == F & column == 'count')) {
      dataframe <-
        dataframe[, lapply(.SD, base::sum), by = c(split_list, 'day_num')]
    }
    dataframe <- dataframe[,.(sim_val = sapply(X = .SD,
                                   FUN = function(data){
                                     if(length(data) < 10){
                                       data <- rep(data,5)
                                     } 
                                     one.boot(data,mean,500,na.rm = T)$t0
                                     })),
                           .SDcols = column,
                           by = split_list]
    return(dataframe)
  }