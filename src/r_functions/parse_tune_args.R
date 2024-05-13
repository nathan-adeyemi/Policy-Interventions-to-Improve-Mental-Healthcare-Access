parse_tune_args <- function(socket,job_name){
  arg_list = list()
  if(grepl('ed-arr',job_name)){
      ed_scale_param <- read_json_con(socket = client_socket)
      if(job_name == 'ed-arr-sep'){
        ed_scale_param <- data.table(fromJSON(ed_scale_param))
      } else{
        ed_scale_param <- as.numeric(ed_scale_param)
      }
      arg_list = list(ed_scale_param = ed_scale_param)
  } else if(job_name == 'accept-prob') {
    acceptance_prob <- data.table(fromJSON(read_json_con(socket = client_socket)))
    colnames(acceptance_prob) = c('Facility_name','prob')
    arg_list = list(ed_scale_param = 1.2, # Optimal ed arrival rate tuning parameter 
                    acceptance_prob_input = acceptance_prob)
  } else {
    arg_list <- fromJSON(read_json_con(socket = client_socket))
  }
  
  return(arg_list)
}