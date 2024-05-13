convert_to_multiple_columns <- function(dataframe, list_column) {
  max_diagnoses <- max(sapply(dataframe[[list_column]], length))  # Find the maximum number of diagnoses
  
  # Create new columns for each diagnosis
  for (i in 1:max_diagnoses) {
    new_column_name <- paste0("diagnosis_", i)
    dataframe[[new_column_name]] <- sapply(dataframe[[list_column]], function(diagnoses) {
      if (length(diagnoses) >= i) {
        return(diagnoses[i])
      } else {
        return(NA)
      }
    })
  }
  
  return(dataframe)
}

# Example usage:
# Assuming your dataframe is called 'df' and the column of lists is named 'diagnosis_list'
# df <- convert_to_multiple_columns(df, "diagnosis_list")
