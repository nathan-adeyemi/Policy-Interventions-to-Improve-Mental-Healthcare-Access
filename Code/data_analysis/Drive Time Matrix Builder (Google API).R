library(gmapsdistance)
library(tictoc)
library(ggmap)

set.api.key(google_maps_api_key)
register_google(google_maps_api_key)

#siteInfo <- read_excel("Data/function_requirements/ip_facilities_info.xlsx")
df <- read_excel("Data/HCCIS/hosplist (1).xlsx",skip = 5) %>% setDT() %>% {function(i) i[,`...1` := NULL]}()
colnames(df)[2] = 'Facility_name'
setDT(df)
df <- df[!is.na(`HCCIS ID`) & !is.na(Facility_name)]
df[,c('lon','lat') := geocode(paste(gsub(pattern = "[*]",replacement = '',Facility_name),Address,City,County,State,Zip,sep = ','))]

df[is.na(lon), c('lon','lat') := geocode('Mayo Clinic in St James')]
# df[grepl('Northfield',Facility_name), c('lon','lat') := geocode('Northfield Hospital, Minnesota')]
df[,dist_info := paste0(lat, "+",lon)]

site.indices <- split(seq_along(df$Facility_name),df$Facility_name) %>% sapply(FUN = function(i) i[1]) %>% as.numeric()
matrices = gmapsdistance(df[site.indices,dist_info],df[site.indices,dist_info],mode = "driving",combinations = "all")
time.matrix = matrices[["Time"]]/3600
distance.matrix = matrices[["Distance"]]/1609.34
# time.matrix = time.matrix[,-1]
# distance.matrix = distance.matrix[,-1]
rownames(time.matrix) <-  rownames(distance.matrix) <- colnames(time.matrix) <- colnames(distance.matrix) <- gsub(pattern = "[*]",replacement = '',df$Facility_name)
saveRDS(time.matrix,file = "./Data/function_requirements/Drive Time Matrix.rds")
saveRDS(distance.matrix, file = "./Data/function_requirements/Drive Distance Matrix.rds")
