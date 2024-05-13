results_density <- function(data){ 
  # results_densityt: creates desnity plot for results
  lapply(seq(8),function(i) ggplot(data[[i]],aes(x = wait_times)) +
           geom_density() +
           scale_x_continuous(limits = c(0,100)) +
           ggtitle(paste0('Wait Time Distribution (Send ',i,' Requests Concurrently)'))) %>%
    grid.arrange(grobs = .,nrow = 4)
}