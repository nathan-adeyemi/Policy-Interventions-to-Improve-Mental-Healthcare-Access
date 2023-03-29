extract_time_validation_metric <- function(data,
           val_group = NA,
           metric = 'disp_to_dep',
           val_func = {
             function(i)
               mean(i, na.rm = T)
           }) {
    
    if (!any(is.na(val_group))) {
      data$val_group <-
        sapply(data[, age_group], function(i)
          any(i == val_group)) %>% data.table()
      if(metric == 'count'){
        data <- rbind(data[,.(count = sum(count)),by = list(val_group,prog_day_I)],data[,.(count = sum(count)),by = list(prog_day_I)],fill = T)
      }
    } else {
      data[, val_group := age_group]
    }
    
    # Type refers to whether patients are Internally placed or Transferred (not all validation data frames have this)
    if (!('type' %in% colnames(data))) {
      data[, type := 'All']
    }
    x <-
      rbindlist(list(data[, .(
        Target = sapply(.SD, function(x) {
          if (!any(c('count','rejections') == metric)) {
            x = remove_outliers(x)
          }
          return(one.boot(x, val_func, 1000)$t0)
        }),
        CI = lapply(.SD, bs),
        Count = .N
      ),
      .SDcols = metric, by = list(val_group, type)],
      data[, .(
        Target =
          sapply(.SD, function(x) {
            if (!any(c('count','rejections') == metric)) {
              x = remove_outliers(x)
            }
            return(one.boot(x, val_func, 1000)$t0)
          }),
        CI = lapply(.SD, bs),
        Count = .N
      ),
      .SDcols = metric, by = list(val_group)],
      data[, .(
        Target =
          sapply(.SD, function(x) {
            if (!any(c('count','rejections') == metric)) {
              x = remove_outliers(x)
            }
            return(one.boot(x, val_func, 1000)$t0)
          }),
        CI = lapply(.SD, bs),
        Count = .N
      ),
      .SDcols = metric, by = list(type)]),
      use.names = T,
      fill = T)[CJ(val_group = val_group,
                   type = type,
                   unique = TRUE),
                on = .(val_group, type)]
    
    if (!('type' %in% colnames(data))) {
      x <- unique(x[, type := NULL])
    }
    return(x[!is.na(Target)])
  }