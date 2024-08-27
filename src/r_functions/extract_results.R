extract_results <- function(df,
                            metric,
                            result_group = NA,
                            result_function = {
                              function(x)
                                mean(x, na.rm = T)
                            },
                            use_type = TRUE,
                            separate_vulnerable = T,
                            collapse_replications = T,
                            resource = F) {
  if (separate_vulnerable) {
    df[, `Vulnerable Patient` := Age != 'Adult', by = list(Sim.ID, replication)]
  } else if (resource) {
    df[, `Vulnerable Patient` :=  resource]
  } else {
    df[, `Vulnerable Patient` := Age]
  }
  if (metric == 'count') {
    df <- df[, .(count = sum(count, na.rm = T)),
             by = list(replication, day_num, `Vulnerable Patient`)]
  }
  if (resource) {
    df <- df[, `:=`(
      treat_adult = grepl(pattern = 'Adult', resource, ignore.case = T),
      treat_child = grepl(pattern = 'Children|Pediatric', resource, ignore.case = T),
      treat_adolescent = grepl(pattern = 'Adolescent', resource, ignore.case = T),
      treat_geriatric = grepl(pattern = 'Geriatric', resource, ignore.case = T)
    )]
    df$Age = tolower(df$Age)
    results_dataframe = rbind(rbindlist(
      lapply(
        X = c('adult', 'adolescent', 'child', 'geriatric'),
        FUN = function(age) {
          eval(parse(
            text = paste0(
              "df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_",
              age,
              ",replication)
         ][treat_",age," == T,
           ][,`:=`(Age = \'",
              age,
              "\',treat_",age," = NULL)]"
            )
          ))
        }
      ),
      fill = TRUE,
      use.names = TRUE
    ),
    
    df[, .(`Simulation Confidence Interval` =
             sapply(
               X = .SD,
               FUN = function(i)
                 one.boot(data = i,
                          FUN = result_function,
                          R = 500)$t0
             )),
       .SDcols = metric,
       by = list(replication)][, `:=`(Age = 'All')],
    fill = T)
    results_dataframe <-
      results_dataframe[, .(
        Mean = one.boot(
          data = `Simulation Confidence Interval`,
          FUN = mean,
          R = 500,
          na.rm = T
        )[['t0']],
        `Simulation Confidence Interval` =
          ci_as_text(signif(
            x = t.test(`Simulation Confidence Interval`, conf.level  = .95)$conf.int,
            digits = 4
          ))
      ), by = Age]
  } else  {
    results_dataframe <-
      rbindlist(
        lapply(
          X = Filter(Negate(is.null), list(
            c('Vulnerable Patient', 'replication'),
            c('replication', `if`(use_type, 'type', NULL)),
            `if`(
              use_type,
              c('Vulnerable Patient', 'replication', 'type'),
              NULL
            )
          )),
          FUN = boot_fn,
          stat = result_function,
          column = metric,
          use_type = use_type,
          dataframe = df,
          
        ),
        fill = TRUE
      )
      
      if (collapse_replications){
        results_dataframe <- results_dataframe[, .(
        Mean = one.boot(
          data = sim_val,
          FUN = mean,
          R = 500,
          na.rm = T
        )[['t0']],
        `Simulation Confidence Interval` = ci_as_text(signif(
          t.test(sim_val,
                 conf.level  = .95)$conf.int,
          4
        ))
      ),
      by = Filter(Negate(is.null), list("Vulnerable Patient" = `Vulnerable Patient`, "type" = `if`(use_type, type, NULL)))]
      }
    if (use_type) {
      results_dataframe <-
        results_dataframe[CJ(
          `Vulnerable Patient` = `Vulnerable Patient`,
          type = type,
          unique = TRUE
        ),
        on = .(`Vulnerable Patient`, type)]
    }
  }
  return(results_dataframe[rowSums(is.na(results_dataframe)) < ncol(results_dataframe)])
}