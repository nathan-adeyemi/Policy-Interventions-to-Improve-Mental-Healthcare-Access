saPlotFun <- function(inputData, sa_metric) {
  ggplot(
    data = inputData,
    mapping = aes(
      x = factor,
      y = value,
      group = `Patient Wait Metric`,
      linetype = `Patient Wait Metric`,
      color = `Patient Wait Metric`
    )) + 
    geom_point(alpha = 0.1) + 
    geom_smooth() +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab(paste('Percentage of Baseline',sa_metric)) +
    ylab('% of Baseline Simulation\'s\n Patient Wait Metric') +
    labs(color = 'Patient Wait Metric',linetype = 'Patient Wait Metric')
}