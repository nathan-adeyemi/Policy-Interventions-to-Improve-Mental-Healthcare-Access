baseline_analysis <- function(results, warmup = 50){
  
    warmup <- warmup * 50
    patients <- results[[1]][, treatment_delay := total_wait_time + Travel.time][Enter.Timestamp > warmup,]
    resources <- results[[2]][time > warmup, ]
    
    baseline_results = list(
    average_treatment_delay = extract_results(
      df = patients,
      metric = 'treatment_delay',
      use_type = T,
      separate_vulnerable = T
    ),
    average_Coordination_Time = 
      extract_results(
        df = patients,
      metric = 'total_wait_time',
      use_type = T,
      separate_vulnerable = T
    ), 
    median_treatment_delay = extract_results(
      df = patients,
      metric = 'treatment_delay',
      result_function = function(data) median(x = data, na.rm  = T),
      use_type = T,
      separate_vulnerable = T
    ),
    median_Coordination_Time = 
      extract_results(
        df = patients,
        result_function = function(data) median(x = data, na.rm  = T),
        metric = 'total_wait_time',
        use_type = T,
        separate_vulnerable = T
      ),
    percent_transfer = extract_results(
      df = patients[, is_transfer := type == 'Transfer'],
      metric = 'is_transfer',
      use_type = F,
      separate_vulnerable = T
    ),
    ip_occupancy = extract_results(
      df = resources[,`:=`(Occupancy = 100 * (server/capacity))],
      metric = 'Occupancy',
      use_type = F,
      separate_vulnerable = F,
      resource = T
    )
  )
    distances <- c(10,25,50)
    baseline_results$transfer_distances <-
    lapply(X = distances,
    FUN = function(miles){
        eval(parse(text = paste0("
                    transfer_within_",miles," = extract_results(
                    df = patients[type == 'Transfer'][, `:=`(in_range = Travel.Distance <= ",miles,")],
                    metric = \'in_range\',
                    use_type = F,
                    separate_vulnerable = T)"
                    )))
            })

    baseline_results$transfer_distances$furthest_transfers <- eval(parse(text = 
        paste0("extract_results(df = patients[type == 'Transfer'][, `:=`(in_range = Travel.Distance > ",max(distances),")],metric = \'in_range\',use_type = F,separate_vulnerable = T)")
    ))

    baseline_results$occupancy_by_resource = resources[,dwell_time := (time - data.table::shift(time, type = 'lag')), by = list(replication, resource)
              ][, Occupancy :=  server/capacity
                ][, .(Occupancy = weighted.mean(x =Occupancy, w = dwell_time, na.rm = T)), by = list(resource, replication)
                  ][,.(Occupancy = mean(Occupancy)), by = resource]

  return(baseline_results)
}