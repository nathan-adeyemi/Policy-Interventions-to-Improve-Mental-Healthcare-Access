library(gmapsdistance)
library(tictoc)
library(ggmap)

set.api.key("AIzaSyBTwMj--xFhVR4iSTl335-5iig6lpgRxZc")
register_google("AIzaSyBTwMj--xFhVR4iSTl335-5iig6lpgRxZc")

#siteInfo <- read_excel("Data/Function Requirements/Rates5.xlsx")
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
saveRDS(time.matrix,file = "./Data/Function Requirements/Drive Time Matrix.rds")
saveRDS(distance.matrix, file = "./Data/Function Requirements/Drive Distance Matrix.rds")
