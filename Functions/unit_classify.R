unit_classify <- function(x) {
  if (grepl('RST ROGE 03 E BEH|Geriatric/Med Psych', x, ignore.case = T)) {
    return('Geriatric/Med Psych')
  } else if (grepl('RST ROGE 01 W BEH|Child/Adolescent Psych', x, ignore.case = T)) {
    return('Child/Adolescent Psych')
  } else if (grepl('RST ROGE 02 E BEH|Adult Psych', x, ignore.case = T)) {
    return('Adult Psych')
  } else if (grepl('RST ROGE 03 W BEH|Mood Disorders Unit', x, ignore.case = T)) {
    return('Mood Disorders Unit')
  } else {
    return('ED')
  }
}