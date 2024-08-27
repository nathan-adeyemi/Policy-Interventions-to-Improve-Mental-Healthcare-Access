library(readxl)
library(data.table)

# Define the list of files
file_list <- c(
  "Results/validation/acceptance-probs",
  "Results/validation/acceptance-probs-Coord",
  "Results/validation/acceptance-probs-norm"
)

# Initialize an empty list to store data tables
valid_files <- list()

# Loop through each file in the file list
for (file in file_list) {
  # Define the column name based on the file
  column <- switch(
    file,
    "Results/validation/acceptance-probs" = 'HCCIS-admissions',
    "Results/validation/acceptance-probs-Coord" = "Median-Coordination-Times",
    "Results/validation/acceptance-probs-norm" = "Min-Max-Normalized-Median-Coordination-and-Admissions"
  )
  
  file_path <-
    # file.path(file, 'trainable-Incumbent/validation_frames.xlsx')
    "Results/validation/ed_factor_ED_Admissions/auto_accept/validation_results.xlsx"
  
  # Get the sheet names
  sheet_names <- excel_sheets(file_path)
  
  # Initialize an empty list to store data tables for this file
  dt_list <- list()
  
  # Read each sheet and add the sheet name as a column
  for (sheet_name in sheet_names) {
    dt <- as.data.table(read_excel(file_path, sheet = sheet_name))
    # Standardize column names to lowercase
    setnames(dt, tolower(names(dt)))
    dt[, `validation-metric-group` := sheet_name]
    dt_list <- append(dt_list, list(dt))
  }
  
  # Combine all sheets into one data table
  combined_dt <- rbindlist(dt_list, fill = TRUE)
  combined_dt[, `RMSE-Tuned-Metric` := column]
  
  # Add the combined data table to the list
  valid_files <- append(valid_files, list(combined_dt))
}

# Concatenate all data tables into a single data table
final_dt <-
  rbindlist(valid_files)[, error := abs(target - sim_val) / target * 100]
final_dt <- split(final_dt, by = 'validation-metric-group')

final_dt <- lapply(
  X = final_dt,
  FUN = function(dt) {
    na_cols <- names(dt)[sapply(dt, function(col)
      all(is.na(col)))]
    return(dt[, (na_cols) := NULL])
  }
)


# Create separate excel sheets for the statewide admissions and facility admissions
final_dt[[9]] <- final_dt[[8]][facility == 'All', ]
final_dt[[8]] <- final_dt[[8]][facility != 'All', ]
names(final_dt)[9] <- 'Statewide Admissions'


piv <- final_dt[[8]]
piv[,`facility size` :=
      cut(
        target,
        breaks = c(0, 100, 500, 1500, Inf),
        labels = c(
          'Small (0-100)',
          'Medium (100-500)',
          'Large (500-1500)',
          'X-Large (1500+)'
        ),
        right = T
      )]
piv <- piv[,.(`Avg. % Error` = mean(error), `Avg Admissions Delta` = mean(`95_ci_delta`)), by = list(`facility size`, grouping,`RMSE-Tuned-Metric`)]
final_dt[[10]] <- piv
names(final_dt)[10] <- 'Admissions Errors by Facility Size'

writexl::write_xlsx(x = final_dt, path = "Results/validation/combined_incumbents.xlsx")




# Code for converting the raw HCCIS admissions errors into a readble table by segmentiang facilities by their number of admissions
# dt[,`facility size` :=
#       cut(
#         admissions,
#         breaks = c(0, 100, 500, 1500, Inf),
#         labels = c(
#           'Small (0-100)',
#           'Medium (100-500)',
#           'Large (500-1500)',
#           'X-Large (1500+)'
#         ),
#         right = T
#       )][,.(Count = .N, `Avg. Error` =mean(error)), by = list(grouping,`facility size`)][grouping == 'total_admissions'][]