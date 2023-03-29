weekday_factors <- function(data){
  rbind(data[,.(N = sum(N),day_of_week = unique(day_of_week)),by = Date
             ][,.(AvgAdmits = mean(N)),by = day_of_week],
        data[,.(N = sum(N),day_of_week = unique(day_of_week)),by = Date
             ][,.(AvgAdmits = mean(N))],fill = T
  )[,factor := AvgAdmits/(.SD[is.na(day_of_week),AvgAdmits])
    ][!is.na(day_of_week)
      ][order(day_of_week)]
}