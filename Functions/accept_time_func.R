accept_time_func <- function(x,y){
  if(length(unique(na.omit(x))) != 0){
    return(max(unique(x),na.rm = T))
  }else{
    return(max(y,na.rm = T))
  }
}