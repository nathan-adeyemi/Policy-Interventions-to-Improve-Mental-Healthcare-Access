
# R Code for plotting various analyses results (because I have nowhere else to put these)

# Plotting the lambda sensitivitty analysis results

lambda_SA_df <-
  data.table(read.csv(
    "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Results/lambda-sensitivity-analysis/grid-search-results.csv"
  ))[,X:=NULL]
names(lambda_SA_df) <- gsub("\\.\\.|\\."," ", names(lambda_SA_df))
plot_df <-
  melt(
    lambda_SA_df,
    id.vars = c(
      'replication',
      'arrival_rate_sa_factor',
      'Vulnerable Patient',
      'type'
    )
  )[, `:=`(`Vulnerable Patient` = as.logical(`Vulnerable Patient`))][,value := value/.SD[arrival_rate_sa_factor == 1, mean(value)], by = list(`Vulnerable Patient`,type,variable)]
split_plot <-
  split(plot_df,
        by = c('type', 'Vulnerable Patient'),
        flatten = F)

plt <- saPlotFun(
  inputData =  split_plot$Transfer$`TRUE`[grepl('Median',variable),],
  x_ax = 'arrival_rate_sa_factor',
  y_ax = 'value',
  grouping_var = 'variable',
  sa_metric = 'Arrival Rate'
)

ggsave(filename = file.path("Results","lambda-sensitivity-analysis","arrival_rate_sensitivity_analysis.jpeg"),
       plot = plt)


# Plotting the Length of Stay sensitivity analysis reuslts
los_SA_df <-
  data.table(read.csv(
    file.path(
      "Results",
      "los-sensitivity-analysis",
      "grid-search-results.csv"
    )
  ))[,X:=NULL] #[los_sa_factor >= 0.75 & los_sa_factor <= 1.25]
names(los_SA_df) <- gsub("\\."," ", names(los_SA_df))
plot_df <-
  melt(
    los_SA_df,
    id.vars = c(
      'replication',
      'los_sa_factor',
      'Vulnerable Patient',
      'type'
    )
  )[, `:=`(`Vulnerable Patient` = as.logical(`Vulnerable Patient`))][,value := value/.SD[los_sa_factor == 1, mean(value)], by = list(`Vulnerable Patient`,type,variable)]
split_plot <-
  split(plot_df,
        by = c('type', 'Vulnerable Patient'),
        flatten = F)

plt <- saPlotFun(
  inputData =  split_plot$Transfer$`TRUE`[grepl('Median',variable),],
  x_ax = 'los_sa_factor',
  y_ax = 'value',
  grouping_var = 'variable',
  sa_metric = 'Length of Stay Rate'
)

ggsave(filename = file.path("Results","los-sensitivity-analysis","LoS_analysis.jpeg"),
       plot = plt)