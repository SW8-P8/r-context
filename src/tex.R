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

get_clic_impact_on_rank_tex <- function(df){
  combined_results <- get_combined_clic_model(df)
  results_df <- combined_results[!rownames(combined_results) %in% c("4|3", "3|2", "2|1"), ]  
  
  rownames(results_df) <- dplyr::recode(rownames(results_df),
                                              "cred_score" = "Credibility",
                                              "like_score" = "Likeability",
                                              "info_score" = "Informativeness",
                                              "clar_score" = "Clarity")
  results_df <- results_df %>%
    dplyr::mutate(
      Value = round(Value, 3),
      Std..Error = round(Std..Error, 3),
      t.value = round(t.value, 3),
      p.value = ifelse(p.value < 0.001, "< 0.001", round(p.value, 3))
    )
  
  latex_str <- capture.output(print(xtable(results_df), type = "latex"))
  return(latex_str)
}
