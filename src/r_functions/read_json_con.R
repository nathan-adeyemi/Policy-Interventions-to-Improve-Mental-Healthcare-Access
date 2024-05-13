read_json_con <- function(socket, max_length = 256){
  json_string <- ""
  
  while(TRUE){
    new_piece <- read.socket(socket)
    json_string <- paste0(json_string, new_piece)
    if(nchar(new_piece) < max_length){
      break
    }
  }
  return(json_string)
}
