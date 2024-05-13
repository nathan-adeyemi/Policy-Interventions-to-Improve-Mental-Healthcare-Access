source_classify <- function(origin){
  if(origin == 'Transfer from a Hospital (Different Facility)'){
    return('External Transfer')
  }else if(origin == 'ED'){
    return('ED')
  } else {
    return('Other')
  }
}