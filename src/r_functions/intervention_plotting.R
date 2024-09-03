plot_intervention_combs <-
  function(interventions_df, intervention_num) {
    unique_combinations <-
      unique(interventions_df[, .(`Vulnerable Patient`, type, variable)])
    
    # Loop through each combination and create the plot
    for (comb in 1:nrow(unique_combinations)) {
      vulnerable_patient <-
        (unique_combinations[comb, `Vulnerable Patient`])
      type_value <- unique_combinations[comb, type]
      metric_name <- unique_combinations[comb, variable]
      # browser()
      plot <- intBoxPLotFn(inputData = interventions_df[(`Vulnerable Patient` == vulnerable_patient) &
                                                          (type == type_value)][variable == metric_name],
                           y_ax = metric_name,
                           grouping_var = 'concurrent_requests',
                           plot_title = paste(vulnerable_patient,
                                                    `if`(grepl('Transfer', type_value, ignore.case = T), "Transferred Admissions", "Internal Admissions")))
                                               
      
      # Create a unique filename for each plot
      filename <-
        paste0(
          "Results/interventions/intervention-",
          intervention_num,
          "/",
          ifelse(vulnerable_patient, "Vulnerable", "NonVulnerable"),
          "-",
          type_value,
          "-",
          metric_name,
          "-boxplot.jpeg"
        )
      # Save the plot
      ggsave(
        filename = filename,
        plot = plot,
        width = 7,
        height = 4,
        device = 'jpeg',
        dpi = 700
      )
    }
  }

intBoxPLotFn <- function(inputData, y_ax, grouping_var, plot_title = NA_character_) {
  ggplot(
    data = inputData,
    mapping = aes_string(
      x = grouping_var,
      y = 'value',
      group = grouping_var,
      fill = grouping_var
    )
  ) +
    geom_boxplot() +
    xlab("Number of Transfer Requests Sent Concurrently") +
    ylab(y_ax) +
    theme_linedraw() +
    theme(legend.position = "none") + 
    ggtitle(plot_title)
}
