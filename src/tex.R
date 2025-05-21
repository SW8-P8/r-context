library(xtable)

source("src/stats.R")

get_clic_repeated_measures_anova_tex <- function(df) {
    anova_results <- get_clic_repeated_measures_anova_results(df)
    anova_table <- as.data.frame(anova_results$anova_table)
    anova_table <- cbind(Effect = rownames(anova_table), anova_table)
    rownames(anova_table) <- NULL
    latex_str <- capture.output(
      print(
        xtable(anova_table),
        type = "latex",
        floating = FALSE  # <-- removes \begin{table}
      )
    )
    return(paste(latex_str, collapse = "\n"))
}

get_clic_pairwise_prototype_t_test_tex <- function(df) {
  results <- get_clic_pairwise_prototype_t_test_results(df)
  
  # Extract matrix of p-values
  p_matrix <- as.matrix(results$p.value)
  
  # Optional: format p-values (e.g., scientific notation)
  formatted_matrix <- formatC(p_matrix, format = "e", digits = 2)
  
  # Convert matrix to data frame for xtable
  df_matrix <- as.data.frame(formatted_matrix)
  df_matrix <- cbind(Comparison = rownames(df_matrix), df_matrix)
  rownames(df_matrix) <- NULL
  
  # Convert to LaTeX
  latex_str <- capture.output(print(xtable(df_matrix), type = "latex", include.rownames = FALSE, floating = FALSE))
return(paste(latex_str, collapse = "\n"))
}

get_clic_descriptive_stats_tex <- function(df) {
  results <- get_clic_descriptive_stats_results(df)
  
  # Convert to LaTeX
  latex_str <- capture.output(
    print(
      xtable(results, caption = "Descriptive statistics by prototype", label = "tab:descriptive_stats"),
      type = "latex",
      include.rownames = FALSE,
      floating = FALSE
    )
  )
  
  return(paste(latex_str, collapse = "\n"))
}

get_clic_tukey_tex <- function(df) {
  # Run ANOVA and get pairwise emmeans contrast results
  anova_results <- get_clic_repeated_measures_anova_results(df)
  data_long <- get_clic_long_data(df)
  
  library(emmeans)
  emm <- emmeans(anova_results, ~ prototype)
  pairwise_results <- contrast(emm, method = "pairwise", adjust = "tukey")
  
  # Convert to data frame
  pairwise_df <- as.data.frame(summary(pairwise_results))
  
  # Optional: clean up column names and round values
  colnames(pairwise_df) <- c("Comparison", "Estimate", "SE", "df", "t", "p.value")
  pairwise_df <- pairwise_df %>%
    dplyr::mutate(
      Estimate = round(Estimate, 3),
      SE = round(SE, 3),
      t = round(t, 2),
      p.value = ifelse(p.value < 0.001, "< 0.001", round(p.value, 3))
    )
  
  # Convert to LaTeX table
  latex_str <- capture.output(
    print(xtable(pairwise_df), type = "latex", include.rownames = FALSE, floating = FALSE)
  )
  
  return(paste(latex_str, collapse = "\n"))
}


get_rank_clic_polr_tex <- function(df){
  combined_results <- get_rank_clic_polr_results(df)
  results_df <- combined_results[!rownames(combined_results) %in% c("4|3", "3|2", "2|1"), ]  
  
  rownames(results_df) <- dplyr::recode(rownames(results_df),
                                              "cred_score" = "Credibility",
                                              "info_score" = "Informativeness")  
  results_df <- results_df %>%
    dplyr::mutate(
      Value = round(Value, 3),
      Std..Error = round(Std..Error, 3),
      t.value = round(t.value, 3),
      p.value = ifelse(p.value < 0.001, "< 0.001", round(p.value, 3))
    )
  
  latex_str <- capture.output(print(xtable(results_df), type = "latex", floating = FALSE))
  return(latex_str)
  }