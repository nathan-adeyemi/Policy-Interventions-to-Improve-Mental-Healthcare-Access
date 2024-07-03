ED_classify <- function(ed) {
  return(switch(
    ed,
    'MCHS NPNH ED' = 'Mayo Clinic Health System in New Praugue',
    'MCHS ALAH ED' = 'Mayo Clinic Health System - Albert Lea and Austin',
    'MCHS LASF ED' = 'Mayo Clinic Health System in LaCrosse (WI)',
    'MCHS RWMC ED' = 'Mayo Clinic Health System in Red Wing',
    'MCHS AUAH ED' = 'Mayo Clinic Health System - Albert Lea and Austin',
    'MCHS MAMH ED' = 'Mayo Clinic Health System in Mankato',
    'MCHS SRSM ED' = 'Mayo Clinic Health System in Springfield',
    'RST ROMB ED' = 'Mayo Clinic Hospital - Rochester',
    'MCHS EULH ED' = 'Mayo Clinic Health System in Eau Claire (WI)',
    'MCHS LCMC ED' = 'Mayo Clinic Health System in Lake City',
    'MCHS OOOH ED' = 'Mayo Clinic Health System in Owatonna',
    'ZZMCHS SFSH ED' = 'Mayo Clinic Health System in Unknown',
    'MCHS BRBH ED' = 'Mayo Clinic Health System in Unknown',
    'MCHS MEMH ED' = 'Mayo Clinic Health System in Menomonie',
    'MCHS FAFH ED' = 'Mayo Clinic Health System in Fairmont',
    'MCHS WCWE ED' = 'Mayo Clinic Health System in Waseca',
    'MCHS SJSJ ED' = 'Mayo Clinic Health System in St. James',
    'MCHS CACF ED' = 'Mayo Clinic Health System in Cannon Falls',
    'MCHS BLBH ED' = 'Mayo Clinic Health System in Bloomer (WI)',
  ))
}

source_classify <- function(origin){
  if(origin == 'Transfer from a Hospital (Different Facility)'){
    return('External Transfer')
  }else if(origin == 'ED'){
    return('ED')
  } else {
    return('Other')
  }
}

replace_health_systems <- function(df) {
  df$facility_contacted[is.na(df$facility_contacted)] <-
    with(df, receiving_facility[is.na(facility_contacted)])
  for (sys in names(health_systems)) {
    systems <- df[grepl(sys, facility_contacted, ignore.case = T), ]
    if (nrow(systems) > 0) {
      for (row in seq(nrow(systems))) {
        temp <- systems[rep(row, length(health_systems[[sys]]))]
        temp$facility_contacted <- health_systems[[sys]]
        df <- rbind(df, temp)
      }
    }
    df <-  df[!grepl(sys, facility_contacted, ignore.case = T)]
  }
  df[, facility_contacted := tolower(facility_contacted)]
  return(df)
}

sub_NonPrivIP <- function(i){
  return(as.numeric(i %in% siteInfo$Facility_name))
}

accept_time_func <- function(x,y){
  if(length(unique(na.omit(x))) != 0){
    return(max(unique(x),na.rm = T))
  }else{
    return(max(y,na.rm = T))
  }
}


find_drive_distance <- Vectorize(function(x, y) {
  if (length(x) == 0 ||
      length(na.omit(y)) == 0 ||
      !(x %in% colnames(distance.matrix)) ||
      !(y %in% colnames(distance.matrix))) {
    return(NA)
  } else{
    return(distance.matrix[x, na.omit(y)[1]])
  }
})

extract_bed <- function(i){
  return(tail(unlist(strsplit(i,' ')),1))
}

# Simulation Input Calculation Functions --------------------------------------------------------
calc_coordination_time <- function(df){
  no_availability <- df[grepl('no availability|no beds|no capacity|no (.*) availability|no (.*) available|unavailable|no (.*) capacity|no (.*) beds|no (.*) bed|at capacity',
                              add_info,ignore.case = T) | grepl('\\<full\\>',add_info,ignore.case = T),]
  
  df<- setDT(setdiff(df,no_availability))
  fac_p1 <- df$facility_contacted %>% tolower()
  facs <- table(na.omit(fac_p1))
  names <- names(facs)
  facs <- as.numeric(facs)
  names(facs) <- names
  resList = list()
  
  for(i in seq_along(facs)){
    
    test_name <- names(facs)[i]
    
    if (facs[i] > 1){
      inds <-which(sapply(tolower(df$facility_contacted),function(text) test_name == text)) %>% unname() %>% {function(i) c(i[1],i[length(i)])}()
      temp_time = as.numeric(abs(difftime(time1 = df[inds[2],event_ts],
                                          time2 = df[inds[1],event_ts],
                                          units = 'hours')))
      temp_time <- ifelse(temp_time == 0,NA,temp_time)
      
      names(temp_time) <- test_name
      resList <- append(resList,temp_time)
    } else {
      
      inds <- which(sapply(tolower(df$facility_contacted),function(text) test_name == text)) %>% unname()
      temp_time <-  as.numeric(abs(difftime(time1 = df[inds,event_ts],
                                            time2 = df[inds[1],if_else(!is.na(ed_disp_ts),ed_disp_ts,ts_1)],
                                            units = 'hours')))
      
      temp_time <- ifelse(temp_time == 0,NA,temp_time)
      
      names(temp_time) <- test_name
      
      resList <- append(resList,temp_time)
    }
  }
  return(resList)
}

time_extract <- function(t1,t2,t3){
  t2 <- min(c(t2,t3)[!is.infinite(as.numeric(c(t2,t3)))])
  if (t1 == t2){
    return(NA_real_)
  }else{
    return(abs(as.numeric(difftime(t1,t2,units = 'hours'))))
  }
}

timeOfDay <- function(input){
  if( hour(input) >= 0 & hour(input) < 6 ){
    result = 'Early Morning'
  } else if (hour(input) >= 6 & hour(input) < 12){
    result = 'Morning'
  } else if (hour(input) >= 12 & hour(input) < 18){
    result = 'Afternoon/Evening'
  } else {
    result = 'Night'
  }
  return(result)
}

weekday_factors <- function(data){
  rbind(data[,.(N = sum(N),day_of_week = unique(day_of_week)),by = Date
  ][,.(AvgAdmits = mean(N)),by = day_of_week],
  data[,.(N = sum(N),day_of_week = unique(day_of_week)),by = Date
  ][,.(AvgAdmits = mean(N))],fill = T
  )[,factor := AvgAdmits/(.SD[is.na(day_of_week),AvgAdmits])
  ][!is.na(day_of_week)
  ][order(day_of_week)]
}

time_of_day_factors <- function(data){
  rbind(data[,.(AvgAdmits = mean(N)), by = time_of_day],
        data[,.(AvgAdmits = mean(N))],fill = T
  )[,factor := AvgAdmits/(.SD[is.na(time_of_day),AvgAdmits])][!is.na(time_of_day)]
}

# Bootstrapping, Confidence Intervals and other Statistical FUNCTIONctions ------------
boot_confInt_target_val <- function(x, FUNCTION = {function(i) mean(i,na.rm = T)}, interval = 95,interval_type = 'basic'){
  boot_name <- switch(interval_type,
                      'basic' = 'basic',
                      'perc' = 'percent',
                      'norm' = 'normal',
                      'stud' = 'student',
                      'bca' = 'bca')
  p <- boot.ci(one.boot(x,FUNCTION,500),conf = interval * 0.01,type = interval_type)[[boot_name]]
  endpoint_indices <- ifelse(rep(interval_type == 'norm',2), c(2,3),c(4,5))
  
  p <- p[endpoint_indices] %>% {function(x) paste0('(',x[1],",",x[2],")")}()
  return(p)
}

boot_confInt_val <-function(x,
                            FUNCTION = { function(i) mean(i, na.rm = T) },
                            interval = 95,
                            mean.only = FALSE,
                            conf.level = NULL,
                            boot.interval.type = 'basic') {
  
  if(length(x) == 0){
    return(0)
  }else if(length(unique(x)) == 1 & length(x) > 10){
    return(paste(x,' (',length(x),')'))
  } else {
    if(is.null(conf.level)){
      conf.level = interval/100
    }
    result <- one.boot(x,FUNCTION = FUNCTION ,R = 1000) %>%  boot.ci(conf = conf.level,type = boot.interval.type)
    if (!is.null(result) & !mean.only){
      
      switch(boot.interval.type,
             'perc' = 'percent',
             'norm' = 'normal')
      
      result <- list(conf.int = result[[boot.interval.type]][,c(4,5)])
      # %>% as.character() %>%
      #   {function(x) paste0('(',x[1],",",x[2],")")}())  %>% unlist() %>% paste0(collapse = ', ')
    } else if (!is.null(result)){
      result <- result$t0
    }
    return(result)
  }
}

boot_confInt_inputs <-function(x,
                               FUNCTION,
                               interval = 95,
                               mean.only = FALSE,
                               ci.only = FALSE) {
  if(length(unique(x)) == 1 & length(x) > 10){
    return(paste(x,' (',length(x),')'))
  } else {
    result <- one.boot(x,FUNCTION = FUNCTION ,R = 1000) %>%  boot.ci(conf = interval/100,type = 'basic')
    if(!is.null(result)){
      if (mean.only){
        result <- result$t0
      } else if(ci.only){
        result <- result$basic[c(4,5)]  %>% round(digits = 3) %>% as.character() %>%
          {function(x) paste0('(',x[1],",",x[2],")")}() %>% unlist() %>% paste0(collapse = ', ')
      } else {
        result <- list('t0' = result$t0   %>%  round(digits = 3)  %>% as.character(),
                       int_name = result$basic[c(4,5)]  %>% round(digits = 3) %>% as.character() %>%
                         {function(x) paste0('(',x[1],",",x[2],")")}())  %>% unlist() %>% paste0(collapse = ', ')
      }
      return(result)
    }
  }
}

dist_fit <- function(data){
  data <- c(remove_zeros(na.omit(as.numeric(data))))
  if(length(data) >= 10){
    dist_type <- lapply(test_dists(data),function(i) i$aic)
    
    tryCatch(expr = {dist_type <- which.min(dist_type)},
             error = function(e){
               browser()
             })
    fit.estimate <- summary(fitdist(data,names(dist_type)))$estimate
    return(list(parameter_estimates <- fit.estimate))
  } 
}

dist_cutoff <- function(max_quant,distribution){
  data <- inpatient_stays[inpatient_stays < quantile(inpatient_stays,max_quant)]
  data <- sample(data,3000,replace = F)
  return(fitdist(data,distr = distribution)$loglik)
}

test_dists <- function(values){
  if(any((values %% 1) != 0)){
    dists <- c('norm','lnorm','exp')
    # ,'weibull','beta','gamma','unif')
  }else{
    dists <- c('pois','geom','nbinom','hyper','binom')
  }
  x <- lapply(
    X = dists,
    FUN = function(distribution) {
      tryCatch({
        fitdist(data = values, distr = distribution)
      }, error = function(e) {
        
      })
      
    }
  )
  x <- setNames(x,dists)
}

ci_as_text <- function(interval,dec.digits = 3){
  interval <- specify_decimal(interval,digits = dec.digits)
  paste0("(",min(interval),",",max(interval),")")
}
