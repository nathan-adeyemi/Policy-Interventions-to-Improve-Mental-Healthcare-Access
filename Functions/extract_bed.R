extract_bed <- function(i){
  return(tail(unlist(str_split(i,' ')),1))
}
