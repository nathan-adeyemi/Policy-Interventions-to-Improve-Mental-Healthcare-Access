timeOfDay <- function(input){
  if( hour(input) >= 0 & hour(input) < 6 ){
    result = 'Early Morning'
  } else if (hour(input) >= 6 & hour(input) < 12){
    result = 'Morning'
  } else if (hour(input) >= 12 & hour(input) < 18){
    result = 'Afternoon/Evening'
  } else {
    result = 'Night'
  }
  return(result)
}