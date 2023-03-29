extract_results <- function(df,
                            metric,
                            result_group = NA,
                            result_function = {
                              function(x)
                                mean(x, na.rm = T)
                            },
                            use_type = TRUE,
                            separate_vulnerable = T,
                            resource = F) {
  
  if (separate_vulnerable) {
    df[, `Vulnerable Patient` := Age != 'Adult',by = list(Sim.ID,replication)]
  } else if (resource) {
    df[, `Vulnerable Patient` :=  resource]
  } else {
    df[, `Vulnerable Patient` := Age]
  }
  if (metric == 'count'){
    df <- df[, .(count = sum(count,na.rm = T)),
             by = list(replication, day_num, `Vulnerable Patient`)]
  } 
  if (resource){
    df <- df[, `:=`(
      treat_adults = grepl(pattern = 'Adult', resource, ignore.case = T),
      treat_children = grepl(pattern = 'Children|Pediatric', resource, ignore.case = T),
      treat_adolescent = grepl(pattern = 'Adolescent', resource, ignore.case = T),
      treat_geriatric = grepl(pattern = 'Geriatric', resource, ignore.case = T)
    )]
    ret = rbind(
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_adults,replication)
         ][treat_adults == T,
           ][,`:=`(Age = 'Adult',treat_adults = NULL)],
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_adolescent,replication)
         ][treat_adolescent == T,
           ][,`:=`(Age = 'Adolescent',treat_adolescent = NULL)],
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_children,replication)
         ][treat_children == T,
           ][,`:=`(Age = 'Child',treat_children = NULL)],
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_geriatric,replication)
         ][treat_geriatric == T,
           ][,`:=`(Age = 'Geriatric',treat_geriatric = NULL)],
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(replication)
         ][,`:=`(Age = 'All')],
      fill = T)
    ret <- ret[, .(Mean = one.boot(data = `Simulation Confidence Interval`,FUN = mean,R = 500,na.rm = T)[['t0']],`Simulation Confidence Interval` = 
                     ci_as_text(signif(x = t.test(`Simulation Confidence Interval`,conf.level  = .95)$conf.int,
                                       digits = 4))),by = Age]
  }else if (use_type) {
    ret <-
      rbind(df[, .(`Simulation Confidence Interval` = 
                     sapply(
                       .SD,
                       FUN = function(i)
                         one.boot(data = na.omit(i),
                                  FUN = result_function,
                                  R = 500)$t0
                     )), 
               .SDcols = metric, by = list(`Vulnerable Patient`, replication, type)
               ],
            df[, .(`Simulation Confidence Interval` =
                     sapply(
                       .SD,
                       FUN = function(i)
                         one.boot(data = na.omit(i),
                                  FUN = result_function,
                                  R = 500)$t0
                     )),
               .SDcols = metric, by = list(replication, type)], 
            df[, .(`Simulation Confidence Interval` =
                     sapply(
                       .SD,
                       FUN = function(i)
                         one.boot(data = na.omit(i),
                                  FUN = result_function,
                                  R = 500)$t0
                     )),  
                         .SDcols = metric, by = list(`Vulnerable Patient`, replication)
                         ],fill = T)[, .(Mean = one.boot(data = `Simulation Confidence Interval`,FUN = mean,R = 500,na.rm = T)[['t0']],
                                         `Simulation Confidence Interval` = ci_as_text(signif(t.test(`Simulation Confidence Interval`, 
                                                                                                     conf.level  = .95)$conf.int, 4))),
                                     by = list(`Vulnerable Patient`, type)
                                     ][CJ(`Vulnerable Patient` = `Vulnerable Patient`,
                                          type = type,unique = TRUE), 
                                       on = .(`Vulnerable Patient`, type)]
  } else {
    ret <- rbind(df[, .(`Simulation Confidence Interval` = 
                          sapply(.SD, function(x)
                            one.boot(data = na.omit(x),
                                     FUN = result_function,
                                     R = 500)$t0)), 
                    .SDcols = metric, by = list(`Vulnerable Patient`, replication)],
                 df[, .(`Simulation Confidence Interval` = 
                          sapply(.SD, function(x)
                            one.boot(data = na.omit(x),
                                     FUN = result_function,
                                     R = 500)$t0)), 
                    .SDcols = metric, by = list(replication)
                    ],fill = T
    )[, .(Mean = one.boot(data = `Simulation Confidence Interval`,FUN = mean,R = 500,na.rm = T)[['t0']],
          `Simulation Confidence Interval` = ci_as_text(interval = signif(x = t.test(x = `Simulation Confidence Interval`,
                                                                                     conf.level  = .95)$conf.int, 4))),
      by = list(`Vulnerable Patient`)]
  }
  return(ret) 
}