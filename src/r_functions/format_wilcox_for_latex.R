format_wilcox_for_latex <- function(result) {
  latex_output <- paste(
    "\\textit{", result$method, "}\n",
    "Data: ", result$data_name, "\n",
    "W = ", result$statistic, ", p-value = ", format(result$p.value, scientific = TRUE), "\n",
    "Alternative hypothesis: ", result$alternative, "\n",
    "95 percent confidence interval:", "\n",
    paste0("[", result$conf.int[1], ", ", result$conf.int[2], "]"), "\n",
    "Sample estimates: \n",
    "Difference in location = ", result$estimate, "\n","\n","\n",
    sep = ""
  )
  return(latex_output)
}