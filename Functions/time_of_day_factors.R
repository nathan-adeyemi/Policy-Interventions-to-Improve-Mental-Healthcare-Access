time_of_day_factors <- function(data){
  rbind(data[,.(AvgAdmits = mean(N)), by = time_of_day],
        data[,.(AvgAdmits = mean(N))],fill = T
  )[,factor := AvgAdmits/(.SD[is.na(time_of_day),AvgAdmits])][!is.na(time_of_day)]
}