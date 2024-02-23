fix_and_expand_fac_contacted <- function(df) {
  duplicate_and_fill <- function(df_row,date_ts){
    
    df_datetime <-date_ts
    df_date <- as_date(df_datetime)
    info <- sapply(strsplit(df_row,'-')[[1]],str_trim)
    new_date <- as_datetime(info[1],format = "%m/%d/%Y %H%M")
    if(is.na(new_date)){
      new_date <-
        as_datetime(paste(as_date(df_datetime), paste(
          substr(info[1], 1, 2), substr(info[1], 3, 4), sep = ':'
        )), format = "%Y-%m-%d %H:%M")
    }
    
    new_date <- if(new_date > df_datetime) new_date - days(1) else new_date
    new_df = data.table(df)[,`:=`(event_ts = new_date,facility_contacted = info[2],add_info = info[3])]
    
    return(new_df)
  }

  if (length(strsplit(df$facility_contacted,',')[[1]]) > 1){
    if (!is.na(as.numeric(strsplit(strsplit(df$facility_contacted,',')[[1]][1],'-')[[1]][1]))){
    
      contacts <- strsplit(df$facility_contacted,',')[[1]]
      new_df_rows <- lapply(X=contacts,FUN = duplicate_and_fill,date_ts = df$event_ts)
      new_df_rows <- rbindlist(new_df_rows)

      return(new_df_rows)
      }
    } else {
      return (data.table(df))
    }
}