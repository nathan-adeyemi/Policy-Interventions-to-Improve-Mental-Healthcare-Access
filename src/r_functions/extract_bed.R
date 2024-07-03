extract_bed <- function(i){
  return(tail(unlist(strsplit(i,' ')),1))
}
