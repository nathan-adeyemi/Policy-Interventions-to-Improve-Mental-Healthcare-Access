get_sys_env_var <- function(var_name, var_class = 'numeric') {

  # Function that grabs relecant environment variables from the encompassing shell session.
  # Environment variables are added by the parent python process 

  res <- Sys.getenv(var_name)
  if (nchar(res) == 0) {
    return(NULL)
  } else if (var_class == 'numeric') {
    return(as.numeric(res))
  } else if (grepl('datatable|dataframe', var_class)) {
    print(res)
    return(data.table(fromJSON(res)))
  } else {
    return(as.character(res))
  }
}