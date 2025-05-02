library(xtable)

source("src/stats.R")

get_clic_repeated_measures_anova_tex <- function(df) {
  anova_results <- get_clic_repeated_measures_anova_results(df)
  anova_table <- as.data.frame(anova_results$anova_table)
  anova_table <- cbind(Effect = rownames(anova_table), anova_table)
  rownames(anova_table) <- NULL
  latex_str <- capture.output(print(xtable(anova_table), type = "latex"))
  return(paste(latex_str, collapse = "\n"))
}