generate_random_id <- function(length) {
  # Define the character set
  characters <- c(0:9, letters, LETTERS)  # Including digits and lowercase/uppercase letters
  
  # Sample random characters with replacement and concatenate them into an ID
  random_id <- paste0(sample(characters, length, replace = TRUE), collapse = "")
  
  return(random_id)
}
