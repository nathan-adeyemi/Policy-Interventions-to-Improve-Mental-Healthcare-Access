age.classify <- Vectorize(function(x){
  if (x < 12){
    y <- 'Child'
  }else if (x >=12 & x < 18){
    y <- 'Adolescent'
  }else if (x >=18 & x < 65){
    y <- 'Adult'
  }else{
    y <- 'Geriatric'
  }
  return(y)
})

three_ages <- Vectorize(function(i){
  i <- as.character(i)
  test <- sapply(c('Adolescent','Adult','Geriatric'),function(x) grepl(x,i))
  return(all(test))
})

hccis <- 
  data.table(
    readRDS(
      file.path("simulations", "function_requirements", "hccis.rds")))

siteInfo <-
  data.table(
    readxl::read_excel(
      file.path("simulations",
                "function_requirements",
                "Hospitals and Inpatient Units.xlsx"
      )))

siteInfo <-
  full_join(x = siteInfo,
            y = hccis,
            by = c('Facility_name' = 'hccis_id'))[!is.na(Site),]

# Multiply avg patients per day by percentage of each age (Adults or geriatric in overall adult) to find adjusted arrival rates for each age range.
# Only applicable if a site accomodates both age ranges i.e. this happens for Abbott pediatric because it takes both children and adolescents but not for Abbot
# adults since they accomodate adults but not geriatric patients

siteInfo <- siteInfo[,is.Adult := Age %in% c('Adult','Geriatric')
                                 ][,triple := three_ages(Bed_Group)
                                   ][triple == TRUE, `:=` (Admissions = Total.Admissions, 
                                                           Patient.Days = Total.Patient.Days,
                                                           site_IP_factor = Total.Admissions/(hccis[hccis_id == 'Mayo Clinic Hospital - Rochester',Total.Admissions]))
                                     ][Age %in% c('Adult','Geriatric') & triple == FALSE,
                                       `:=` (Admissions = Adult.Admissions,
                                             Patient.Days = Adult.Days,
                                             site_IP_factor = Adult.Admissions/hccis[hccis_id == 'Mayo Clinic Hospital - Rochester',Adult.Admissions])
                                       ][triple == FALSE & Age %in% c('Child','Adolescent'),
                                         `:=` (Admissions = Pediatric.Admissions,
                                               Patient.Days = Pediatric.Days,
                                               site_IP_factor = Pediatric.Admissions/hccis[hccis_id == 'Mayo Clinic Hospital - Rochester',Pediatric.Admissions])
                                         ][,`:=`(arr_lambda = Admissions/(365*24), # Rate of non-ED arrivals is inverse of admissions per hour * percentage  
                                                 LOS_rate = (Patient.Days*24)/Admissions) # Calculate average unit length of stay according
                                             ][, setdiff(colnames(siteInfo), # Remove uneccesary columns
                                                         c('Site','total_beds','Bed_Group','Facility_name','Age','Admissions',
                                                           'Patient.Days','LOS_rate','arr_lambda','site_IP_factor')) := NULL]

siteInfo[Facility_name == siteInfo[is.na(LOS_rate),Facility_name],
         `:=`(Admissions = max(Admissions,na.rm = T),
              Patient.Days = max(Patient.Days,na.rm = T),
              site_IP_factor = max(site_IP_factor,na.rm = T),
              arr_lambda = max(arr_lambda,na.rm = T),
              LOS_rate = max(LOS_rate,na.rm = T))]

# mayo_hccis <- as.numeric(unique(siteInfo,by = 'Bed_Group')[grepl('Rochester',Facility_name)][,.(Total = sum(arr_lambda * 24))])
# factors <- as.numeric(mayo_adjustment_factors[,factors := Avg/mayo_hccis]$factors)
# factors_names <- unlist(mayo_adjustment_factors[,weekday])
# names(factors) <- as.character(factors_names)
# siteInfo$arr_lambda_non_stationary <- lapply(siteInfo$arr_lambda,function(i) i*factors)

# [, Adult.Both := grepl('Adult',Bed_Group) & grepl('Geriatric',Bed_Group)
# ][,Pediatric.Both := grepl('Adolescent',Bed_Group) & grepl('Child',Bed_Group)
# ][adj_arr,arr_factor := ifelse(Adult.Both|Pediatric.Both,Perc,1), on = ('Age == Age.Category')
# ][,Admissions := (ifelse(Age %in% c('Adult','Geriatric'), 
#                          Adult.Admissions, Pediatric.Admissions)) * arr_factor
# ][,Patient.Days := (ifelse(Age %in% c('Adult','Geriatric'),
#                            Adult.Days,Pediatric.Days)) * arr_factor
# ]
writexl::write_xlsx(
  x = siteInfo,
  path = file.path("simulations",
                   "function_requirements",
                   "ip_facilities_info.xlsx"))
saveRDS(
  object = siteInfo,
  file = file.path("simulations",
                   "function_requirements",
                   "ip_facilities_info.rds")
)
