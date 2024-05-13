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