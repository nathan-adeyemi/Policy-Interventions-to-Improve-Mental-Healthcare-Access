remove_subset_rows <- function(df) {
  word_columns <- lapply(X = setdiff(colnames(df),'n'), 
                         FUN =function(i) df[[i]])
  
  # Initialize a vector to store row indices to be removed
  rows_to_remove <- c()
  # Iterate over each row
  for (i in 1:nrow(df)) {
    # Check if any other row has the same count and contains all the words in the current row
    subset_row <- sapply(1:nrow(df), function(j) {
      j != i &&
        abs(df$n[i] - df$n[j]) < 5 &&
        all(na.omit(na.omit(sapply(word_columns,function(col) col[i]))) %in% na.omit(sapply(word_columns,function(col) col[j])))
    })
    
    # If such a row is found, add the current row index to the rows_to_remove vector
    if (any(subset_row,na.rm = T)) {
      
      rows_to_remove <- c(rows_to_remove, i)
    }
  }
  
  # Remove rows_to_remove from the data frame
  df <- df[-rows_to_remove,]
  return(df)
}