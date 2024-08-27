sim_val_subfunction <-
  function(df,
           val_env_df,
           metric,
           val_group = NA,
           val_function = {
             function(x){
               return(mean(x, na.omit = T))
             }
           },
           use_type = TRUE,
           resource_val = F,
           include_outliers = T,
           confidence_level = 0.95,
           results_by_rep = FALSE) {
    
    ci_and_val_fn <- function(df, conf_level = 0.95,split_list) {
      conf_level_name <- paste(conf_level * 100, 'CI', sep = '_')
      # browser()
      df <- eval(parse(text = paste('df[CJ(replication = unique(replication),', paste(sapply(split_list, function(x) paste0(x,"= unique(",x,")")), collapse = ','), '), on = .(replication,',paste(split_list,collapse = ','),')]')))[is.na(sim_val), sim_val := 0]
      eval(parse(
        text = paste0(
          'df = df[,.(`',conf_level_name,'` = ci_as_text(signif(t.test(sim_val, conf.level = ',conf_level,')$conf.int, 4)),
                    `',conf_level_name,'_contains_target` = {Vectorize(validate_fun)}(text = list(signif(t.test(x = sim_val, conf.level = ',conf_level,')$conf.int, 4)),true_val = unique(target)),
                      target = signif(unique(target),3)),
                  by = list(',paste0(split_list,collapse = ","),')
                    ][CJ(',paste0(sapply(split_list,function(item) paste(rep(item,2),collapse = " = ")),collapse = ','),', unique = TRUE),
                      on = .(',paste0(sapply(split_list,function(item) paste(rep(item,2),collapse = " = ")),collapse = ','),')
                    ][!is.na(target),
                      ][, `',conf_level_name,'_Delta` := signif({Vectorize(validate_fun)}(text = `',conf_level_name,'`,true_val = target,differences = T),3)
                        ][,`',conf_level_name,'_percent_error` := `',conf_level_name,'_Delta`/target * 100]'
        )
      ))
      return(df)
    }
    if (!any(is.na(val_group))) {
      df[, `Vulnerable Patient` := fifelse(Age %in% val_group, T, F)]
      if (metric == 'count')
        df <- df[, .(count = sum(count, na.rm = T)),
                 by = list(replication, day_num, `Vulnerable Patient`)]
    } else if (resource_val) {
      df[, `Vulnerable Patient` :=  resource]
    } else {
      df[, `Vulnerable Patient` := Age]
    }
    if (all(length(unique(na.omit(val_env_df$val_group))) == 2, 
            any(grepl('TRUE', unique(val_env_df$val_group))),
            any(grepl('FALSE', unique(val_env_df$val_group))))){
      val_env_df <- val_env_df[,val_group := as.logical(val_group)]
    }

    boot_by_split <-
        rbindlist(lapply(
          X = Filter(Negate(is.null),list(
                c('Vulnerable Patient', 'replication'),
                c('replication', `if`(use_type, 'type',NULL)),
                `if`(use_type,c('Vulnerable Patient', 'replication', 'type'),NULL)
            )),
          FUN = boot_fn,
          dataframe = df,
          stat = val_function,
          column = metric,
          use_type = use_type
        ),
        fill = TRUE)
    if (use_type) {
      boot_by_split <- boot_by_split[val_env_df, target := Target, on = c(`Vulnerable Patient` = 'val_group', type = 'type')]
    } else {
      boot_by_split <- boot_by_split[val_env_df, target := Target, on = c(`Vulnerable Patient` = 'val_group')]
    }

    if(!results_by_rep){
      if (length(confidence_level) == 1) {
        boot_by_split <- ci_and_val_fn(boot_by_split, split_list = `if`(use_type,c('`Vulnerable Patient`','type'),c('`Vulnerable Patient`')))
      } else{
        boot_by_split <- data.table(do.call(
          cbind,
          lapply(X = confidence_level,
                 FUN = ci_and_val_fn,
                 df = boot_by_split,
                 split_list = `if`(use_type,c('`Vulnerable Patient`','type'),c('`Vulnerable Patient`')))
        ))
        unique_cols = unique(names(boot_by_split))
        unique_cols <-
          c(setdiff(unique_cols, sort(unlist(
            lapply((confidence_level * 100),
                   grep,
                   x = unique_cols,
                   value = T
            )
          ))), sort(unlist(
            lapply((confidence_level * 100),
                   grep,
                   x = unique_cols,
                   value = T
            )
          )))
        boot_by_split <- unique(boot_by_split[, ..unique_cols])
      }
    }
    return(boot_by_split)
  }