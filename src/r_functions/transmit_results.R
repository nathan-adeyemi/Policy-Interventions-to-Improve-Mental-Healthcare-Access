transmit_results <-
  function(results_list,
           receiver,
           output_metric,
           trial_path,
           ed_df,
           warmup = 50,
           sim_length = 365,
           validation_frame = NA_character_,
           arg_list) {
    
    RMSE_norm <- function(y_pred,y_true){
      
      y_pred <- (y_pred - min(y_pred))/(max(y_pred) - min(y_pred))
      y_true <- (y_true - min(y_true))/(max(y_true) - min(y_true))
      
      return(sqrt(MSE(as.numeric(y_pred),y_true)))
    }
    
    # Function that return the simulation results to return to the search algorithm
    origin_date <- as.POSIXct("2018-12-01 00:00:00", tz = "UTC") + (3600 * 24 * warmup)
    val_env <- readRDS(file.path("Data","Validation Metric Dataframes.rds"))
    if (grepl(
      'treatment_delay',
      output_metric
    )) {
      patients <- results[[1]]
      patients[,treatment_delay := total_wait_time + Travel.Time]
      results_dict <-
        list(
          average_treatment_delay = extract_results(
            df = patients,
            metric = 'treatment_delay',
            use_type = T,
            collapse_replications = F,
            separate_vulnerable = T
          ),
          average_Coordination_Time = 
            extract_results(
              df = patients,
            metric = 'total_wait_time',
            use_type = T,
            collapse_replications = F,
            separate_vulnerable = T
          ), 
          median_treatment_delay = extract_results(
            df = patients,
            metric = 'treatment_delay',
            result_function = function(data) median(x = data, na.rm  = T),
            use_type = T,
            collapse_replications = F,
            separate_vulnerable = T
          ),
          median_Coordination_Time = 
            extract_results(
              df = patients,
              result_function = function(data) median(x = data, na.rm  = T),
              metric = 'total_wait_time',
              use_type = T,
              collapse_replications = F,
              separate_vulnerable = T
            )
        )

    } else if(grepl('coordination_time',output_metric)){
      res_df <-
        results_list[[1]][, .(
          `Coordination Time` = one.boot(
            total_wait_time,
            FUN = median,
            R = 500,
            na.rm = TRUE
          )$t0),
        by = list(replication, type, `Vulnerable Patient` = Age != 'Adult')
        ][val_env$ed_wait_median, true_val := Target, on = c(`Vulnerable Patient` = 'val_group', type = 'type')]
      
      true_col = "true_val"
      pred_col = "Coordination Time"
      
      results_dict <- list(MSE = MSE(as.numeric(res_df[[pred_col]]),res_df[[true_col]]),
                          RMSE = sqrt(MSE(as.numeric(res_df[[pred_col]]),res_df[[true_col]])),
                          MAE = MAE(as.numeric(res_df[[pred_col]]),res_df[[true_col]]),
                          RAE = RAE(as.numeric(res_df[[pred_col]]),res_df[[true_col]]),
                          RMSE_norm = RMSE_norm(res_df[[pred_col]],res_df[[true_col]]))
      
  } else {
      res_df <- validate_results(patients_df = results_list[[1]],
                                resource_df = results_list[[2]],
                                conf = 0.95,
                                warmup=warmup,
                                sim_days = sim_length,
                                results_by_rep = T)

      if(output_metric != 'All'){
        res_df <- res_df[validation_frame == output_metric]
      }
      true_col = 'target'
      pred_col = 'sim_val'
      results_dict <- list(MSE = MSE(as.numeric(res_df[[pred_col]]),res_df[[true_col]]),
                           RMSE = sqrt(MSE(as.numeric(res_df[[pred_col]]),res_df[[true_col]])),
                           MAE = MAE(as.numeric(res_df[[pred_col]]),res_df[[true_col]]),
                           RAE = RAE(as.numeric(res_df[[pred_col]]),res_df[[true_col]]),
                           RMSE_norm = RMSE_norm(res_df[[pred_col]],res_df[[true_col]]),
                           Replications = max(as.numeric(res_df$replication)))
  }
    write.socket(receiver, jsonlite::toJSON(results_dict, auto_unbox = TRUE))
  }