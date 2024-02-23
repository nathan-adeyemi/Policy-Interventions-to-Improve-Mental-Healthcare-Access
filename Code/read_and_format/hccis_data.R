hccis <- data.table(readxl::read_excel('Data/HCCIS/hccis2020.xlsx')
                    )[,hccis_id := NULL]
hospitals <- data.table(readxl::read_excel('Data/HCCIS/hosplist.xlsx',skip = 5))[,`HCCIS ID` := as.numeric(`HCCIS ID`)]
if("...1" %in% colnames(hospitals)){
   hospitals[,...1 := NULL]
}
formset <- data.table(
  code = c(
    4014,
    8016,
    7509,
    7518,
    4307,
    8034,
    7532,
    7541,
    7464,
    7482,
    7491,
    7000,
    7044,
    4503
  ),
  Name = c(
    'Total.Patient.Days',
    'Adult.ICU.Days',
    'Adult.Non.ICU.Days',
    'Pediatric.Days',
    'Total.Admissions',
    'Adult.ICU.Admissions',
    'Adult.Non.ICU.Admissions',
    'Pediatric.Admissions',
    'Total.Beds',
    'Adult.Beds',
    'Pediatric.Beds',
    'ED.Present',
    'ED.to.IP.Admissions',
    'ED.Registrations'
    )
)
hccis <-  data.table(pivot_wider(data = hccis[formset, on = 'code'][hospitals, hccis_id := `Hospital Name`, on = c('owner_hccis_id' = 'HCCIS ID')][, code := NULL], 
               names_from = Name,
               values_from = value))
hccis <- hccis[,hccis_id := gsub('[*]','',hccis_id)] 
hccis[hccis_id == "Children's Minnesota",c('Total.Beds','Pediatric.Beds')] = 16
hccis[is.na(hccis)] <- 0
hccis[, `:=`(
  Adult.Days = Adult.Non.ICU.Days + Adult.ICU.Days,
  Adult.Admissions = Adult.ICU.Admissions + Adult.Non.ICU.Admissions,
  Adult.Non.ICU.Days = NULL,
  Adult.ICU.Days = NULL,
  Adult.Non.ICU.Admissions = NULL,
  Adult.ICU.Admissions = NULL
)]

saveRDS(
  object = hccis,
  file = file.path("simulations", "function_requirements", "hccis.rds")
)
write_xlsx(x = hccis,
           path = file.path('Data', 'HCCIS', 'hccis_ed_ips_2020.csv'))
