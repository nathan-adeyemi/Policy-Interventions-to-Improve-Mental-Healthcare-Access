parse_tune_args <- function(socket,job_name){

  arg_list = list()
  params <- read_json_con(socket = client_socket)
  

  if(job_name == 'ed-arr-sep'){
    ed_scale_param <- data.table(hccis_id = names(fromJSON(params)),
                                ed_scale_param = fromJSON(params))
  } else if(grepl('acceptance-probs|accept|probs',job_name)) {
    acceptance_prob <- data.table(Facility_name = names(fromJSON(params)),
                                  prob = fromJSON(params))
    arg_list = list(ed_scale_param = 1.22, # Optimal ed arrival rate tuning parameter 
                    acceptance_prob_input = acceptance_prob)
  } else {
    arg_list <- fromJSON(params)
  }
  return(arg_list)
}