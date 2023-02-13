invisible(lapply(c('data.table','readxl','writexl','tidyverse'),library,character.only = T))
#### Resetting the working Directory for the correct system ###
year <- readline(prompt="Input Year:")
year <- as.numeric(year)
if (Sys.info()['user'] == 'adeyemi.n'){
  prefix = 'C:/Users/adeyemi.n'
}else if(Sys.info()['sysname'] == 'Darwin'){
  prefix = '/Users/nadeyemi'
}else{
  prefix = 'Y:'
}
setwd(paste0(prefix,'/OneDrive - Northeastern University/Graduate/Research/Mental Health'))
hccis <- read_excel(paste0('./Data/HCCIS/',year,'/hccis',year,'.xlsx')) %>% data.table()
hospitals <- read_excel(paste0('./Data/HCCIS/hosplist (1).xlsx'),skip = 5) %>% data.table()
if("...1" %in% colnames(hospitals)){
   hospitals[,...1 := NULL]
}
formset <- data.table(Code = c(4014,8016,7509,7518,
                               4307,8034,7532,7541,
                               7464,7482,7491,7000,
                               7044,4503,7689,7236,
                               0721,0727,7240,0714,
                               7546,7238,0724,7132),
                      Name = c('Total.Patient.Days','Adult.ICU.Days','Adult.Non.ICU.Days','Pediatric.Days',
                               'Total.Admissions', 'Adult.ICU.Admissions','Adult.Non.ICU.Admissions','Pediatric.Admissions',
                               'Total.Beds','Adult.Beds','Pediatric.Beds',
                               'ED.Present','ED.to.IP.Admissions','ED.Registrations','Med/Surg Charges',			
                               'Cardiac Charges','Chemical_Dependency_Charges','Mental_Health_Charges','Neurology_Charges',
                               'Neonatal_Care_Charges','Obstetrics_Charges','Orthopedic_Charges','Rehabilitation_Charges','Other_Specialty_Charges'))

mh.colnames <- setdiff(formset$Name,c('ED.Present','ED.to.IP.Admissions','ED.Registrations','Med/Surg Charges',			
                                      'Cardiac Charges','Chemical_Dependency_Charges','Mental_Health_Charges','Neurology_Charges',
                                      'Neonatal_Care_Charges','Obstetrics_Charges','Orthopedic_Charges','Rehabilitation_Charges','Other_Specialty_Charges'))

hccis <- hccis[code %in% formset[,Code],]
hccis$code <- formset[match(hccis$code,formset$Code),Name]
hccis$hccis_id <- hospitals[match(hccis$hccis_id,hospitals$`HCCIS ID`),`Hospital Name`]
hccis %<>% pivot_wider(names_from = code, values_from = value) %>% data.frame()
hccis <- hccis[,c(colnames(hccis[,c(1:3)]),sort(colnames(hccis[,-c(1:3)])))]
setDT(hccis)
hccis[hccis_id == "Children's Health Care dba Children's Hospitals and Clinics of Minnesota****",c('Total.Beds','Pediatric.Beds')] = 16

hccis %>% filter_at(vars(mh.colnames),any_vars(!is.na(.)))
hccis[is.na(hccis)] <- 0
hccis[,Adult.Days := Adult.Non.ICU.Days + Adult.ICU.Days]
hccis[,Adult.Admissions := Adult.ICU.Admissions + Adult.Non.ICU.Admissions]

hccis[,Adult.Non.ICU.Days := NULL]
hccis[,Adult.ICU.Days := NULL]
hccis[,Adult.Non.ICU.Admissions := NULL]
hccis[,Adult.ICU.Admissions := NULL]
setDF(hccis)
charges <- hccis[,which(grepl(pattern = 'Charges',colnames(hccis),ignore.case = T))]
setDT(hccis)
setDT(charges)

charges <- apply(charges,2,FUN = function(i) sum(i,na.rm = T))
charges_names <- names(charges)
charges <- data.table(charges)
charges[,charge_name := charges_names]
hccis[,hccis_id := gsub('[*]','',hccis_id)]
# hccis$Total.Beds[114] <- 12
# hccis$Total.Beds[18] <- 10
# hccis <- hccis[Total.Beds > 0,]

#hccis <- hccis[((hccis[,..mh.colnames] %>% rowSums()) != 0) %>% which(),]

# Fix Ridgeview and Winona beds

# hccis <- hccis |> mutate(Total.Days.Per.Patient = Total.Patient.Days/Total.Admissions,
#                       Adult.Days.Per.Patient = (Adult.ICU.Days + Adult.Non.ICU.Days)/(Adult.ICU.Admissions + Adult.Non.ICU.Admissions),
#                       Pediatric.Days.Per.Patient = (Pediatric.Days + Nursery.Days) / (Pediatric.Admissions + Nursery.Admissions),
#                       ED.Arrivals.Per.Day = ED.to.IP.Admissions/365)
saveRDS(hccis,paste0("./Data/Function Requirements/hccis.rds"))
write_csv(hccis,file = paste0('./Data/HCCIS/',year,'/hccis',year,'Wide.csv'))
write_csv(charges,paste0('./Data/HCCIS/',year,'/hccis_charges',year,'Wide.csv'))
