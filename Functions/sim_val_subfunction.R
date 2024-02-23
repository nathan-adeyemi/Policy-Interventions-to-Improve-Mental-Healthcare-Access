sim_val_subfunction <-
  function(df,
           val_env_df,
           metric,
           val_group = NA,
           val_function = {
             function(x)
               mean(x, na.omit = T)
           },
           use_type = TRUE,
           resource_val = F,
           include_outliers = T,
           confidence_level = 0.95) {
    if (!any(is.na(val_group))) {
      df[, `Vulnerable Patient` := fifelse(Age %in% val_group, T, F)]
      if (metric == 'count')
        df <- df[, .(count = sum(count,na.rm = T)),
                 by = list(replication, day_num, `Vulnerable Patient`)]
    } else if (resource_val){
      df[,`Vulnerable Patient` :=  resource]
    } else {
      df[, `Vulnerable Patient` := Age]
    }
    if (use_type) {
      ret <-
        rbind(df[, .(`Simulation Confidence Interval` = 
                       sapply(
                         .SD,
                         FUN = function(i)
                           one.boot(`if`(include_outliers,i,remove_outliers(i)),
                                    FUN = val_function,
                                    R = 500)$t0
                       )), 
                 .SDcols = metric, by = list(`Vulnerable Patient`, replication, type)
                 ],df[, .(`Simulation Confidence Interval` = 
                            sapply(
                              .SD,
                              FUN = function(i)
                                one.boot(`if`(include_outliers,i,remove_outliers(i)),
                                         FUN = val_function,
                                         R = 500)$t0
                            )), 
                      .SDcols = metric, by = list(replication, type)
                      ],df[, .(`Simulation Confidence Interval` = 
                                 sapply(
                                   .SD,
                                   FUN = function(i)
                                     one.boot(`if`(include_outliers,i,remove_outliers(i)),
                                              FUN = val_function,
                                              R = 500)$t0
                                 )), 
                           .SDcols = metric, by = list(`Vulnerable Patient`, replication)
                           ],fill = T)[val_env_df, `Target Value` := Target, on = c(`Vulnerable Patient` = 'val_group', type = 'type')
                                       ][!is.na(`Target Value`),][, .(`Simulation Confidence Interval` = ci_as_text(signif(t.test(`Simulation Confidence Interval`, conf.level = confidence_level)$conf.int, 4)),
                                                                      `Target Value` = unique(`Target Value`),
                                                                      `Statistically Valid?` = t.test(x = `Simulation Confidence Interval`,mu = unique(`Target Value`))$p.value > (1-confidence_level),
                                                                      `CI contains True Value?` = {Vectorize(validate_fun)}(text = list(signif(t.test(`Simulation Confidence Interval`, conf.level = confidence_level)$conf.int, 4)), 
                                                                                                                            true_val = unique(`Target Value`))),
                                                                  by = list(`Vulnerable Patient`, type)
                                                                  ][CJ(`Vulnerable Patient` = `Vulnerable Patient`,type = type,unique = TRUE), on = .(`Vulnerable Patient`, type)
                                                                    ][!is.na(`Target Value`),][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                                                                                                    true_val = `Target Value`,
                                                                                                                                    differences = T)]
    } else {
      ret <- rbind(df[, .(`Simulation Confidence Interval` = 
                            sapply(.SD, function(x)
                              one.boot(`if`(include_outliers, na.omit(x),remove_outliers(na.omit(x))),
                                       FUN = val_function,
                                       R = 500)$t0)), 
                      .SDcols = metric, by = list(`Vulnerable Patient`, replication)],
                   df[,.(count = sum(count)), by = list(replication,day_num)
                      ][, .(`Simulation Confidence Interval` = 
                              sapply(.SD, function(x)
                                one.boot(`if`(include_outliers, na.omit(x),remove_outliers( na.omit(x))),
                                         FUN = val_function,
                                         R = 500)$t0)), 
                        .SDcols = metric, by = list(replication)
                        ],fill = T)[val_env_df, `Target Value` := Target, on = c(`Vulnerable Patient` = 'val_group')
                                    ][,.(`Simulation Confidence Interval` = ci_as_text(signif(t.test(`Simulation Confidence Interval`, conf.level = confidence_level)$conf.int, 4)),
                                         `Target Value` = unique(`Target Value`),
                                         `Statistically Valid?` = t.test(x = `Simulation Confidence Interval`,
                                                                         mu = unique(`Target Value`))$p.value > (1-confidence_level),
                                         `CI contains True Value?` = {Vectorize(validate_fun)}(text = list(signif(t.test(`Simulation Confidence Interval`, conf.level = confidence_level)$conf.int, 4)),
                                                                                               true_val = unique(`Target Value`))),
                                      by = list(`Vulnerable Patient`),
                                      ][CJ(`Vulnerable Patient` = `Vulnerable Patient`, unique = TRUE), 
                                        on = .(`Vulnerable Patient`)
                                        ][!is.na(`Target Value`),
                                          ][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                                                 true_val = `Target Value`,
                                                                                 differences = T)]
    }
    return(ret) 
  }