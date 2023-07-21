saPlotFun <- function(inputData) {
  ggplot(
    data = inputData,
    mapping = aes(
      x = factor,
      y = value,
      group = `Patient Wait Metric`,
      linetype = `Patient Wait Metric`,
      color = `Patient Wait Metric`
    )
  ) + geom_point() + geom_smooth() +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    xlab('Percentage of Baseline ED Arrival Rate') +
    ylab('% of Baseline Simulation\'s\nWait Time (in hrs.)') +
    labs(color = 'Patient Wait Metric',linetype = 'Patient Wait Metric')
}