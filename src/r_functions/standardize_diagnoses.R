standardize_diagnosis <- function(dataframe, diagnosis_column) {
  # Define mappings of similar diagnoses to the same category
  diagnosis_mappings <- list(
    'suicidal ideation' = c("SI", "S.I.", "suicide ideation", "suicidal ideation", "suicide attempt","suicidal ideations", "suicidal ideations w a plan"),
    'homicide' = c("HI", "H.I.", "homicidal ideation", "homicidal attempt"),
    'overdose' = c('overdose',"od","o.d.")
    # Add more categories and associated diagnoses as needed
  )
  
  # Function to standardize individual diagnosis
  standardize_single_diagnosis <- function(diagnosis) {
    diagnosis <- tolower(diagnosis)  # Convert to lowercase for case insensitivity
    # Loop through diagnosis mappings and check if the diagnosis matches any category
    for (category in names(diagnosis_mappings)) {
      if (any(diagnosis %in% tolower(diagnosis_mappings[[category]]))) {
        return(category)
      }
    }
    return(diagnosis)  # If no match found, return the original diagnosis
  }
  
  # Function to split diagnoses and standardize each one
  split_and_standardize <- function(diagnoses) {
    diagnoses <- unlist(strsplit(diagnoses, ",|;|and|/"))  # Split diagnoses
    diagnoses <- trimws(diagnoses)  # Remove leading/trailing whitespace
    diagnoses <- lapply(diagnoses, standardize_single_diagnosis)  # Standardize each diagnosis
    return(diagnoses)
  }
  
  # Apply split_and_standardize function to the diagnosis column
  dataframe$standard_diagnosis <- lapply(dataframe[[diagnosis_column]], split_and_standardize)
  
  return(dataframe)
}

# Example usage:
# Assuming your dataframe is called 'df' and the diagnosis column is named 'diagnosis'
# df <- standardize_diagnosis(df, "diagnosis")
