transmit_results <- function(res_df,receiver,tune_job){

  # Function that return the simulation results to return to the search algorithm
  if(tune_job == 'ed-arr'){
    res_df <- res_df[facility == 'All'] 
    res_df <- unique(res_df[,!('grouping'),with = FALSE])
  } else if (grepl('ed-arr-sep|accept-prob',tune_job)){
    # Return the number of admissions df w/o the 'ALL' category
    res_df <- res_df[facility != 'All']
    res_df <- unique(res_df[,!('grouping'),with = FALSE])
  }

  write.socket(receiver, toJSON(res_df,dataframe='columns'))
}