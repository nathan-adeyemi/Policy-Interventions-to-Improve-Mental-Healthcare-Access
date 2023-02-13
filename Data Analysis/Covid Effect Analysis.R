if(!any(sapply(c('ed_bh_admits','ed_transfers','transferred_admits'),exists))){
  source(file.path(".","Code",'Simulation Project',"Data Analysis","Read and Format Mayo Data.R"))
}


#  Covid Effects on Transferred Patients  -------------------------------------------------------------------------

# Differences in post disposition ED LoS
transfer_time_aov <- kruskal.test(
  formula = disp_to_dep ~ pre_Covid + age_group + interaction(age_group, pre_Covid),
  data = ed_transfers
)

transfer_time_summary <- summary.aov(transfer_time_aov)
leveneTest(
  disp_to_dep ~ pre_Covid + age_group + interaction(age_group, pre_Covid),
  data = ed_transfers
)

# Differences in number of rejections
summary.aov(aov(rejections ~ pre_Covid, data = ed_transfers))
