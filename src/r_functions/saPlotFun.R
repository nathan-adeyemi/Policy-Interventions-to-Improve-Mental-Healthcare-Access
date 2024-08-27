saPlotFun <- function(inputData, x_ax, y_ax, grouping_var, sa_metric) {
  ggplot(
    data = inputData,
    mapping = aes(
      x = !!sym(x_ax),
      y = !!sym(y_ax),
      grouping = !!sym(grouping_var),
      color = !!sym(grouping_var),
    )) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(se = FALSE, aes(linetype = !!sym(grouping_var))) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_color_brewer(palette = "Dark2") +
    xlab(paste('Percentage of Baseline',sa_metric)) +
    ylab('% of Baseline Simulation\'s\n Patient Delay Metric') +
    labs(color = 'Patient Delay Metric',linetype = 'Patient Delay Metric') + 
    theme_linedraw() +
    theme(legend.position = "bottom") 
    # facet_wrap(~ variable, nrow = 2)
}
