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

combined_unit <- Vectorize(function(i){
  i <- as.character(i)
  test <- sapply(c('Adolescent','Adult','Geriatric'),function(x) grepl(x,i))
  return(all(all(c(sum(test) >= 2, grepl('Adolescent',i)))))
})

hccis <- 
  data.table(
    readRDS(
      file.path("src/simulations", "function_requirements", "hccis.rds")))

siteInfo <-
  data.table(
    readxl::read_excel(
      file.path("src/simulations",
                "function_requirements",
                "Hospitals and Inpatient Units.xlsx"
      )))

siteInfo <-
  full_join(x = siteInfo,
            y = hccis,
            by = c('Facility_name' = 'hccis_id'))[!is.na(total_beds),]

# Multiply avg patients per day by percentage of each age (Adults or geriatric in overall adult) to find adjusted arrival rates for each age range.
# Only applicable if a site accomodates both age ranges i.e. this happens for Abbott pediatric because it takes both children and adolescents but not for Abbot
# adults since they accomodate adults but not geriatric patients
siteInfo <- siteInfo[,is.Adult := Age %in% c('Adult','Geriatric')
                                 ][,combined_unit := combined_unit(Bed_Group)
                                   ][Age %in% c('Adult','Geriatric') & combined_unit == FALSE,
                                       `:=` (site_IP_factor = Adult_Admissions/hccis[hccis_id == 'Mayo Clinic Hospital - Rochester',Adult_Admissions],
                                             LOS_rate = (Adult_Days*24)/Adult_Admissions) # Calculate average unit length of stay according)
                                       ][combined_unit == FALSE & Age %in% c('Child','Adolescent'),
                                         `:=` (site_IP_factor = Pediatric_Admissions/hccis[hccis_id == 'Mayo Clinic Hospital - Rochester',Pediatric_Admissions],
                                               LOS_rate = (Pediatric_Days *24)/Pediatric_Admissions)
                                        ][combined_unit == TRUE , `:=` (site_IP_factor = Total_Admissions/(hccis[hccis_id == 'Mayo Clinic Hospital - Rochester',Total_Admissions]),
                                                      LOS_rate = (Total_Patient_Days*24)/Total_Admissions) # Calculate average unit length of stay according)
                                          ][, setdiff(colnames(siteInfo), # Remove uneccesary columns
                                                              c('Site','total_beds','Bed_Group','Facility_name','Age',
                                                                'Patient_Days','LOS_rate','arr_lambda','site_IP_factor')) := NULL
                                                                ][unique(empirical_dist[,list(age,age_mean_rate)]),
                                                                  mayo_los := age_mean_rate,on = c('Age' = 'age')
                                                                  ][,`:=`(mayo_scale_param = mayo_los/LOS_rate,mayo_los = NULL)
                                                                    ][grepl('Mayo Clinic Hospital - Rochester',Facility_name),mayo_scale_param := 1
                                                                      ][order(Facility_name),Site := seq(.N)]


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
  path = file.path("src/simulations",
                   "function_requirements",
                   "ip_facilities_info.xlsx"))
saveRDS(
  object = siteInfo,
  file = file.path("src/simulations",
                   "function_requirements",
                   "ip_facilities_info.rds")
)
