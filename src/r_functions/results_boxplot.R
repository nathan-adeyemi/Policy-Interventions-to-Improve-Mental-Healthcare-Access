results_boxplot <- function(data,col){ 
  # results_boxplot: creates boxplots for results
  ggplot(data,aes(x = log2(wait_times),y = col, group = col)) +
    geom_boxplot() +
    rotate() +
    scale_x_continuous(limits = c(-3.75,7))
}