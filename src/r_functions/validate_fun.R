validate_fun <- function(text,
                         true_val,
                         data = NA,
                         alpha = 0.05,
                         differences = F) {
  if (!is.na(data)) {
    # T.test for if the simulation distribution contains the true parameter
    if (length(data) < 10) {
      data <- rep(data, 100000000)
    }
    return(tryCatch({
      t.test(data, mu = true_val)$p.value > alpha
    },
    error = function(e) {
      err <- cat("ERROR :", conditionMessage(e))
      return (err)
    }))
  } else{
    if (is.character(text)) {
      values <-
        str_split(text, ",|\\(|\\)| ")[[1]] %>%
        as.numeric() %>%
        na.omit()
    } else if (is.list(text) | is.numeric(text)) {
      values <- unlist(text)
    }
    
    min_val <- min(values)
    max_val <- max(values)
    
    if (differences == T) {
      if((true_val > max_val)){
        return(max_val - true_val)
      } else if (true_val < min_val) {
        return(min_val - true_val)
      } else {
        return(0)
      }
    } else {
      return(true_val <= max_val &
               true_val >= min_val)
    }
  }
}