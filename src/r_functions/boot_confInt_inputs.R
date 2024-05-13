boot_confInt_inputs <-function(x,
                     FUNCTION,
                     interval = 95,
                     mean.only = FALSE,
                     ci.only = FALSE) {
    if(length(unique(x)) == 1 & length(x) > 10){
      return(paste(x,' (',length(x),')'))
    } else {
      result <- one.boot(x,FUNCTION = FUNCTION ,R = 1000) %>%  boot.ci(conf = interval/100,type = 'basic')
      if(!is.null(result)){
        if (mean.only){
          result <- result$t0
        } else if(ci.only){
          result <- result$basic[c(4,5)]  %>% round(digits = 3) %>% as.character() %>%
            {function(x) paste0('(',x[1],",",x[2],")")}() %>% unlist() %>% paste0(collapse = ', ')
        } else {
          result <- list('t0' = result$t0   %>%  round(digits = 3)  %>% as.character(),
                         int_name = result$basic[c(4,5)]  %>% round(digits = 3) %>% as.character() %>%
                           {function(x) paste0('(',x[1],",",x[2],")")}())  %>% unlist() %>% paste0(collapse = ', ')
        }
        return(result)
      }
    }
  }

dist_fit <- function(data){
  data <- as.numeric(data)
  if(length(na.omit(data)) >= 10){
    dist_type <- names(which.min(mclapply(test_dists(data),function(i) i$aic)))
    fit.estimate <- summary(fitdist(data,dist_type))$estimate
    return(list(parameter_estimates <- fit.estimate))
  }else{
    return(list(mean(data)))
  }
}

dist_cutoff <- function(max_quant,distribution){
  data <- inpatient_stays[inpatient_stays < quantile(inpatient_stays,max_quant)]
  data <- sample(data,3000,replace = F)
  return(fitdist(data,distr = distribution)$loglik)
}

test_dists <- function(values){
  if(any((values %% 1) != 0)){
    dists <- c('norm','lnorm','exp')
    # ,'weibull','beta','gamma','unif')
  }else{
    dists <- c('pois','geom','nbinom','hyper','binom')
  }
  x <- lapply(
    X = dists,
    FUN = function(distribution) {
      tryCatch({
        fitdist(data = values, distr = distribution)
      }, error = function(e) {
        
      })
      
    }
  )
  x <- setNames(x,dists)
}

ci_as_text <- function(interval,dec.digits = 3){
  interval <- specify_decimal(interval,digits = dec.digits)
  paste0("(",min(interval),",",max(interval),")")
}
