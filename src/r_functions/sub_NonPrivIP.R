sub_NonPrivIP <- function(i){
  return(as.numeric(i %in% siteInfo$Facility_name))
}