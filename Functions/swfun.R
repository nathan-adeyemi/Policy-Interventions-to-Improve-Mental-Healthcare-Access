swfun <- function(i){
  if(grepl('Mayo',i)){
    return(switch(i,
                  "Mayo Rochester Pediatric/Adolescent" = "Child/Adolescent Psych",
                  "Mayo Rochester Adult" = "Adult Psych",
                  "Mayo Rochester Geriatric" = "Geriatric/Med Psych"))
  } else {
    return(switch(i,
                  "Adolescent" = "Child/Adolescent Psych",
                  "Child" = "Child/Adolescent Psych",
                  "Adult" = "Adult Psych",
                  "Geriatric" = "Geriatric/Med Psych"))
  }
}