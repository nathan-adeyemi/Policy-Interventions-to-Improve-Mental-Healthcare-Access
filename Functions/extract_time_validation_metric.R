extract_time_validation_metric <-
  function(data,
           val_group = NA,
           metric = 'disp_to_dep',
           use_type = FALSE,
           val_func = mean,
           ...) {
    add_args = list(...)
    sub_fn <- function(data, ci = FALSE, stat, arg_list) {
      if ('probs' %in% names(arg_list)) {
        probs = arg_list$probs
      }
      
      if ('na.rm' %in% names(arg_list)) {
        remove_na = arg_list$na.rm
      }
      
      if (ci) {
        if (identical(stat, DescTools::Quantile)) {
          est_stat = DescTools::QuantileCI(x = data,
                                           probs = probs,
                                           na.rm = remove_na)
          est_stat = ci_as_text(est_stat[c('lwr.ci', 'upr.ci')])
        } else if (identical(stat, mean)) {
          est_stat = t.test(data)$conf.int
        } else{
          est_stat = bs(data, FUNCTION = stat)
        }
      } else{
        if (identical(stat, DescTools::Quantile)) {
          est_stat = DescTools::Quantile(data, probs = probs, na.rm = remove_na)
        } else if (identical(stat, mean)) {
          est_stat = t.test(data)$estimate
        } else {
          est_stat = stat(data)
        }
      }
      
      return(est_stat)
    }
    
    if (!any(is.na(val_group))) {
      # browser()
      data$val_group = data$age_group != 'Adult'
      if (metric == 'count') {
        data <-
          rbind(data[, .(count = sum(count)), by = list(val_group, prog_day_I)],
                data[, .(count = sum(count)), by = list(prog_day_I)],
                fill = T)
      }
    } else {
      data[, val_group := age_group]
    }
    
    if (!use_type) {
      split_list <-  'val_group'
    } else{
      split_list <- list(c('type', 'val_group'), 'type', 'val_group')
    }
    
    browser()
    df_list <- lapply(
      X = split_list,
      FUN = function(split)
        data[, .(
          Target = sapply(.SD, sub_fn, stat = val_func, arg_list = add_args),
          CI = lapply(
            X = .SD,
            FUN = sub_fn,
            stat = val_func,
            ci = TRUE,
            arg_list = add_args
          ),
          Count = .N
        ), .SDcols = metric, by = split]
    )
    
   if(use_type){
      df_list <- c(df_list, list(data[, .(
        Target = sapply(.SD, sub_fn, stat = val_func, arg_list = add_args),
        CI = lapply(
          X = .SD,
          FUN = sub_fn,
          stat = val_func,
          ci = TRUE,
          arg_list = add_args
        ),
        Count = .N
      ), .SDcols = metric]))
   }
    
    
    df <- rbindlist(df_list,
                    fill = TRUE,
                    use.names = TRUE)
    
    return(df[!is.na(Target)])
  }