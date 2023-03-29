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
