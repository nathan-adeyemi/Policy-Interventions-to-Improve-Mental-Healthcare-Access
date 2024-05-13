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