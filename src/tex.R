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
  
  # Rename prototype values
  prototype_mapping <- c(
    "baselineClic" = "Instagram's Default",
    "descClic" = "Content Description",
    "warnClic" = "Trigger Warnings",
    "drawingClic" = "Drawing Filter"
  )
  results$prototype <- prototype_mapping[results$prototype]
  
  # Make column headers bold
  colnames(results) <- c(
    "\\textbf{Prototype}", 
    "\\textbf{Mean}", 
    "\\textbf{SD}", 
    "\\textbf{Min}", 
    "\\textbf{Max}"
  )
  
  # Convert to LaTeX
  latex_str <- capture.output(
    print(
      xtable(results, caption = "Descriptive statistics by prototype", label = "tab:descriptive_stats"),
      type = "latex",
      include.rownames = FALSE,
      floating = FALSE,
      sanitize.colnames.function = identity  # Keep LaTeX formatting
    )
  )
  
  return(paste(latex_str, collapse = "\n"))
}



get_clic_tukey_tex <- function(df) {
  pairwise_results <- get_clic_tukey_results(df)
  
  # Convert to data frame
  pairwise_df <- as.data.frame(summary(pairwise_results))
  
  # Replace prototype codes with 2-letter names in Comparison column
  replacements <- c(
    "baselineClic" = "ID",
    "descClic" = "CD",
    "warnClic" = "TW",
    "drawingClic" = "DF"
  )
  for (code in names(replacements)) {
    pairwise_df$contrast <- gsub(code, replacements[[code]], pairwise_df$contrast)
  }
  
  # Keep only the desired columns (drop df)
  pairwise_df <- pairwise_df %>%
    dplyr::select(contrast, estimate, SE, t.ratio, p.value) %>%
    dplyr::rename(Comparison = contrast) %>%
    dplyr::mutate(
      Estimate = round(estimate, 3),
      SE = round(SE, 3),
      t = round(t.ratio, 2),
      p.value = ifelse(p.value < 0.001, "< 0.001", round(p.value, 3))
    ) %>%
    dplyr::select(Comparison, Estimate, SE, t, p.value)
  
  # Rename and bold headers
  colnames(pairwise_df) <- c(
    "\\textbf{Comparison}",
    "\\textbf{EMD}",
    "\\textbf{SE}",
    "\\textbf{t}",
    "\\textbf{p.value}"
  )
  
  # Convert to LaTeX
  latex_str <- capture.output(
    print(
      xtable(pairwise_df),
      type = "latex",
      include.rownames = FALSE,
      floating = FALSE,
      sanitize.colnames.function = identity
    )
  )
  
  return(paste(latex_str, collapse = "\n"))
}



get_rank_clic_polr_tex <- function(df) {
  combined_results <- get_rank_clic_polr_results(df)
  
  # Remove threshold rows
  results_df <- combined_results[!rownames(combined_results) %in% c("4|3", "3|2", "2|1"), ]
  
  # Rename rownames for clarity
  rownames(results_df) <- dplyr::recode(
    rownames(results_df),
    "cred_score" = "Credibility",
    "info_score" = "Informativeness"
  )
  
  # Convert rownames to column
  results_df <- tibble::rownames_to_column(as.data.frame(results_df), var = "Predictor")
  
  # Format columns
  results_df <- results_df %>%
    dplyr::mutate(
      Value = round(Value, 3),
      Std.Error = round(Std..Error, 3),
      t = round(t.value, 3),
      p.value = ifelse(p.value < 0.001, "< 0.001", round(p.value, 3))
    ) %>%
    dplyr::select(Predictor, Value, Std.Error, t, p.value)
  
  # Rename and bold column headers
  colnames(results_df) <- c(
    "\\textbf{Predictor}",
    "\\textbf{Estimate}",
    "\\textbf{SE}",
    "\\textbf{t}",
    "\\textbf{p.value}"
  )
  
  # Convert to LaTeX
  latex_str <- capture.output(
    print(
      xtable(results_df),
      type = "latex",
      include.rownames = FALSE,
      floating = FALSE,
      sanitize.colnames.function = identity
    )
  )
  
  return(paste(latex_str, collapse = "\n"))
}
