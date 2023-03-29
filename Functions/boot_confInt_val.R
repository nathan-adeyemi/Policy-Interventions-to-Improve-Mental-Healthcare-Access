boot_confInt_val <-function(x,
                            FUNCTION = { function(i) mean(i, na.rm = T) },
                            interval = 95,
                            mean.only = FALSE,
                            conf.level = NULL,
                            boot.interval.type = 'basic') {
  
  if(length(x) == 0){
    return(0)
  }else if(length(unique(x)) == 1 & length(x) > 10){
    return(paste(x,' (',length(x),')'))
  } else {
    if(is.null(conf.level)){
      conf.level = interval/100
    }
    result <- one.boot(x,FUNCTION = FUNCTION ,R = 1000) %>%  boot.ci(conf = conf.level,type = boot.interval.type)
    if (!is.null(result) & !mean.only){
      
      switch(boot.interval.type,
             'perc' = 'percent',
             'norm' = 'normal')
      
      result <- list(conf.int = result[[boot.interval.type]][,c(4,5)])
      # %>% as.character() %>%
      #   {function(x) paste0('(',x[1],",",x[2],")")}())  %>% unlist() %>% paste0(collapse = ', ')
    } else if (!is.null(result)){
      result <- result$t0
    }
    return(result)
  }
}
