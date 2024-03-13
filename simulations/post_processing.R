post_process_arrival_output <-
  function(df, providers_df, ed_df) {
    format_attr_df <-  function(i) {
      dcast(i[grepl('Transfer', name)][nchar(name) != 0, `:=`(Sim.ID = paste(name, replication, sep = '_Rep_'))][!(grepl('Prev_Check_Unit_', key, ignore.case = T)), ][order(list(time, value), decreasing = T)][, .SD[1,], by = list(Sim.ID, replication, key)],
            Sim.ID + replication ~ key, value.var = 'value')[ed_df[, site_num := as.numeric(rownames(.SD))], ed.nm := hccis_id, on = c(Site.Entered = 'site_num')][siteInfo, transfer.nm := Facility_name, on = c(Transfer.Site = 'Site')][, `:=`(Site.Entered = ed.nm, Transfer.Site = transfer.nm)][, `:=`(ed.nm = NULL, transfer.nm  = NULL)][, Age := c('Adolescent', 'Adult', 'Child', 'Geriatric')[Age]][, `:=`(
              Age = as.factor(Age),
              replication = as.numeric(replication),
              type = ifelse(
                Site.Entered != Transfer.Site |
                  is.na(Transfer.Site),
                'Transfer',
                'Internal'
              )
            )][order(replication, Enter.Timestamp)
               ][, `:=`(total_wait_time = Transfer.Site.Found.Timestamp - Enter.Timestamp)
                 ][providers_df,  `:=`(unit = Bed_Group), on = c(Transfer.Trajectory = 'trajectory_num')]
    }
    if (length(unique(df$replication)) == 1) {
      timestamps = format_attr_df(df)
    } else {
      timestamps <-
        rbindlist(mclapply(
          X = split(df, df$replication),
          FUN = format_attr_df,
          mc.cores = ifelse(
            test = length(rep) != 1,
            yes = availableCores(),
            no = 1
          )
        ))
    }
    return(timestamps)
  }

post_process_externals <- function(attribute_df, hospital_df){
  sub_fun <- function(i) {
    dcast(
      data = i[grepl('IP_Unit', name), ]
      [nchar(name) != 0, `:=`(Sim.ID = paste(name, replication, sep = '_Rep_'))][, time := NULL],
      formula = name + replication ~ key,
      value.var = 'value'
    )[hospital_df,  `:=`(unit = Bed_Group), on = c(Transfer.Site = 'Site')]
  }

  if(length(unique(attribute_df$replication))==1){
    externals <- sub_fun(attribute_df)
  } else{
    externals <-
      rbindlist(mclapply(
        X = split(attribute_df, attribute_df$replication),
        FUN = sub_fun
      ),
      fill = T,
      use.names = T)
  }

  return(externals)
}

post_process_resource_output <-
  function(df, attribute_df, arrivals_df, hospital_df){

    sub_fun <- function(i) {

      curr_rep <- unique(df$replication)

      res <- lapply(
        X = unique(hospital_df$Bed_Group),
        FUN = function(test_unit){

            df <- i[resource == test_unit][,cap_change := system - data.table::shift(system,type = 'lag'),by = resource][is.na(cap_change),cap_change := 1]
            increases <- df[cap_change > 0,]
            decreases <- df[cap_change < 0,]

            increases <- increases[resource == test_unit][externals[unit == test_unit], `:=`(
              patient = name,
              `ED Patient` = F,
              origin = NA_character_,
              dest = NA_character_
            ), on = c('time' = 'IP.Arrival.Timestamp', 'replication' = 'replication'), roll = 'nearest'][arrivals_df[unit == test_unit], `:=`(
              patient = Sim.ID,
              `ED Patient` = T,
              origin = Site.Entered,
              dest = Transfer.Site,
              actual_start = IP.Arrival.Timestamp
            ), on = c('time' = 'Enter.Timestamp', 'replication' = 'replication'), roll = 'nearest'][, `:=`(
              `External Transfer` = origin != dest,
              origin = NULL,
              dest = NULL
            )][`ED Patient` == T & !is.na(actual_start),time := actual_start][,actual_start := NULL]

            decreases <- decreases[resource == test_unit][externals[unit == test_unit], `:=`(
              patient = name,
              `ED Patient` = F,
              origin = NA_character_,
              dest = NA_character_
            ), on = c('time' = 'Finish.Timestamp', 'replication' = 'replication'), roll = 'nearest'][arrivals_df[unit == test_unit], `:=`(
              patient = Sim.ID,
              `ED Patient` = T,
              origin = Site.Entered,
              dest = Transfer.Site
            ), on = c('time' = 'Finish.Timestamp', 'replication' = 'replication'), roll = 'nearest'][, `:=`(
              `External Transfer` = origin != dest,
              origin = NULL,
              dest = NULL
            )]

            res <- rbind(increases,decreases)

            return(res)
        })
      res <- rbindlist(res)
      res <- res[order(time)][is.na(`External Transfer`), `External Transfer` := FALSE]
    }
  externals <- post_process_externals(attribute_df, hospital_df)
  if (length(unique(df$replication)==1)){
    resources <- sub_fun(i=df)
  }
  resources <-
    rbindlist(mclapply(
      X = split(df, df$replication),
      FUN = sub_fun,
      mc.cores = ifelse(
        test = length(rep) != 1,
        yes = availableCores(),
        no = 1
      )
    ))
  return(resources)
  }