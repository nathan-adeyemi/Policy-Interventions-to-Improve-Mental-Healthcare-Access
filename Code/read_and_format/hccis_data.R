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
    'Total_Patient_Days',
    'Adult_ICU_Days',
    'Adult_Non_ICU_Days',
    'Pediatric_Days',
    'Total_Admissions',
    'Adult_ICU_Admissions',
    'Adult_Non_ICU_Admissions',
    'Pediatric_Admissions',
    'Total_Beds',
    'Adult_Beds',
    'Pediatric_Beds',
    'ED_Present',
    'ED_to_IP_Admissions',
    'ED_Registrations'
    )
)
hccis <-  data.table(pivot_wider(data = hccis[formset, on = 'code'][hospitals, hccis_id := `Hospital Name`, on = c('owner_hccis_id' = 'HCCIS ID')][, code := NULL], 
               names_from = Name,
               values_from = value))
hccis <- hccis[,hccis_id := gsub('[*]','',hccis_id)] 
hccis[hccis_id == "Children's Minnesota",c('Total_Beds','Pediatric_Beds')] = 16
hccis[is.na(hccis)] <- 0
hccis[, `:=`(
  Adult_Days = Adult_Non_ICU_Days + Adult_ICU_Days,
  Adult_Admissions = Adult_Non_ICU_Admissions + Adult_ICU_Admissions
)]

saveRDS(
  object = hccis,
  file = file.path("simulations", "function_requirements", "hccis.rds")
)
write_xlsx(x = hccis,
           path = file.path('Data', 'HCCIS', 'hccis_ed_ips_2020.csv'))
