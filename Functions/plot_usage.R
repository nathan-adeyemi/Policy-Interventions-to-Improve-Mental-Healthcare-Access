plot_usage <- function(resources,patients){
  resource_by_fac <- split(resources,f = resources$resource)
  resource_by_fac <- lapply(resource_by_fac,function(df){
    class(df) = c('resources','data.frame')
    return(df)
  })
  
  fac_usage_plots <- lapply(resource_by_fac,FUN = function(data) plot(data,metric = 'usage'))
  ggsave( # Makes a pdf of all resource usage plots
    filename = file.path(".",'Simulation and Alternatives','Validation Results') %>%
      file.path(.,list.files(.)[length(list.files(.))]) %>% file.path(.,list.files(.)[length(list.files(.))],"IP Unit Utilization Plots.pdf"),
    plot = marrangeGrob(fac_usage_plots, nrow=1, ncol=1),
    width = 15, height = 9)
}