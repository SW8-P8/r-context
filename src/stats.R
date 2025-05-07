library(MASS)
library(ggplot2)
library(effects)
library(dplyr)
library(tidyr)
library(car)
library(afex)
library(ggsignif)
library(ARTool)
library(PlackettLuce)
library(nortest)
library(ez)

get_clic_long_data <- function(df) {
  data_long <- df %>%
    pivot_longer(cols = c("baselineClic", "descClic", "warnClic", "drawingClic"),  # select columns starting with 'clic'
                 names_to = "prototype",      # name for prototype column
                 values_to = "score")        # name for the score column
  return(data_long)
}

get_clic_shapiro_results <- function(df) {
  data_long <- get_clic_long_data(df)
  shapiro_test_results <- data_long %>%
    group_by(prototype) %>%
    summarise(shapiro_p_value = shapiro.test(score)$p.value) %>%
    arrange(shapiro_p_value) %>%
    mutate(normal = ifelse(shapiro_p_value > 0.05, "Yes", "No"))
  return(shapiro_test_results)
}

get_clic_kolmogorov_results <- function(df) {
  data_long <- get_clic_long_data(df)
  ks_test_results <- data_long %>%
    group_by(prototype) %>%
    summarise(ks_p_value = lillie.test(score)$p.value) %>%
    arrange(ks_p_value) %>%
    mutate(normal = ifelse(ks_p_value > 0.05, "Yes", "No"))
  return(ks_test_results)
}

get_clic_anderson_darling_results <- function(df) {
  data_long <- get_clic_long_data(df)
  ad_test_results <- data_long %>%
    group_by(prototype) %>%
    summarise(p_value_ad = ad.test(score)$p.value) %>%
    arrange(p_value_ad) %>%
    mutate(normal = ifelse(p_value_ad > 0.05, "Yes", "No"))
  return(ad_test_results)
}

get_clic_levenes_results <- function(df) {
  data_long <- get_clic_long_data(df)
  
  data_long$p_seq <- as.factor(data_long$p_seq)
  
  data_avg <- data_long %>%
    group_by(id, p_seq) %>%
    summarize(score = mean(score), .groups = "drop")
  
  levene_results <- leveneTest(score ~ p_seq, data = data_avg)
  
  return(levene_results)
}

get_clic_repeated_measures_anova_results <- function(df) {
  data_long <- get_clic_long_data(df)
  
  data_long$p_seq <- as.factor(data_long$p_seq)
  data_long$prototype <- as.factor(data_long$prototype)
  data_long$id <- as.factor(data_long$id)
  data_long$gender <- as.factor(data_long$gender)
  
  anova_results <- aov_ez(id = "id", 
                         dv = "score", 
                         data = data_long, 
                         within = "prototype", 
                         between = "p_seq")
  
  return(anova_results)
}

get_clic_pairwise_prototype_t_test_results <- function(df) {
  data_long <- get_clic_long_data(df)
  pairwise_results <- pairwise.t.test(data_long$score, data_long$prototype, p.adjust.method = "bonferroni")
  return(pairwise_results)
}

get_clic_pairwise_p_seq_t_test_results <- function(df) {
  data_long <- get_clic_long_data(df)
  pairwise_results <- pairwise.t.test(data_long$score, data_long$p_seq, p.adjust.method = "bonferroni")
  return(pairwise_results)
}

###########################################z############################

get_rank_long_data <- function(df) {
  rank_long <- df %>%
    pivot_longer(cols = c("ranking.1.", "ranking.2.", "ranking.3.", "ranking.4."),
                 names_to = "rank_position",
                 values_to = "prototype") %>%
    mutate(rank = as.numeric(gsub("ranking\\.", "", rank_position))) %>%
    dplyr::select(id, p_seq, prototype, rank)
  return(rank_long)
}

get_rank_matrix_data <- function(df) {
  rank_long <- get_rank_long_data(df)
  rank_matrix <- rank_long %>%
    pivot_wider(names_from = prototype, values_from = rank) %>%
    dplyr::select(-id, -p_seq)
  return(rank_matrix)
}

get_rank_friedman_results <- function(df) {
  rank_long <- get_rank_long_data(df)
  
  rank_long$p_seq <- as.factor(rank_long$p_seq)
  rank_long$prototype <- as.factor(rank_long$prototype)
  
  friedman_result <- friedman.test(rank ~ prototype | id, data = rank_long)
}

get_rank_anova_results <- function(df) {
  rank_long <- get_rank_long_data(df)
  
  rank_long$p_seq <- as.factor(rank_long$p_seq)
  rank_long$prototype <- as.factor(rank_long$prototype)
  
  art_model <- art(rank ~ prototype * p_seq + (1|id), data = rank_long)
  anova_results <- anova(art_model)
  return(anova_results)
}

get_rank_wilcoxon_results <- function(df) {
  rank_long <- get_rank_long_data(df)
  wilcoxon_result <- pairwise.wilcox.test(rank_long$rank, rank_long$prototype, paired = TRUE, p.adjust.method = "bonferroni")
  return(wilcoxon_result)
}

get_rank_placketluce_results <- function(df) {
  rank_matrix <- get_rank_matrix_data(df)
  pl_model <- PlackettLuce(rank_matrix)
  pl_results <- summary(pl_model)
  return(pl_results)
}

get_rank_placketluce_coef_results <- function(df) {
  rank_matrix <- get_rank_matrix_data(df)
  pl_model <- PlackettLuce(rank_matrix)
  # Extract coefficients
  coefficients <- coef(pl_model)
  
  # Exponentiate the coefficients to get odds ratios
  odds_ratios <- exp(coefficients)
  
  # Normalize the odds ratios to sum to 1
  normalized_weights <- odds_ratios / sum(odds_ratios)

  return(normalized_weights)
}

################################################

# Utility to reshape ranking data
get_participant_id_and_rankings <- function(df) {
  df %>%
    dplyr::select(participant_id = id, ranking.1., ranking.2., ranking.3., ranking.4.) %>%
    pivot_longer(cols = starts_with("ranking."),
                 names_to = "rank_position",
                 values_to = "design") %>%
    mutate(rank = as.numeric(gsub("ranking\\.", "", rank_position))) %>%
    dplyr::select(participant_id, design, rank)
}

# Generic function to reshape a Web-CLIC dimension
get_clic_dimension_long <- function(df, prefix, new_var) {
  long_df <- df %>%
    dplyr::select(participant_id = id,
           !!paste0(prefix, "baseline") := !!sym(paste0("baseline", prefix)),
           !!paste0(prefix, "desc") := !!sym(paste0("desc", prefix)),
           !!paste0(prefix, "warn") := !!sym(paste0("warn", prefix)),
           !!paste0(prefix, "drawing") := !!sym(paste0("drawing", prefix))) %>%
    pivot_longer(cols = everything()[-1],
                 names_to = "design_label",
                 values_to = new_var) %>%
    mutate(design = gsub(paste0("^", prefix), "", design_label),
           design = gsub("drawing", "draw", design),
           design = gsub("baseline", "insta", design)) %>%
    dplyr::select(participant_id, design, !!sym(new_var))
  return(long_df)
}

# Function to run correlation + ordinal logistic regression
analyze_clic_effect_on_rank <- function(df, dimension_prefix, score_col_name) {
  rank_data <- get_participant_id_and_rankings(df)
  dimension_data <- get_clic_dimension_long(df, prefix = dimension_prefix, new_var = score_col_name)
  
  merged_data <- left_join(rank_data, dimension_data, by = c("participant_id", "design"))
  
  # Spearman Correlation
  spearman_result <- cor.test(merged_data$rank, merged_data[[score_col_name]], method = "spearman")
  
  # Ordinal Logistic Regression
  merged_data$rank <- factor(merged_data$rank, levels = 4:1, ordered = TRUE)
  model <- MASS::polr(as.formula(paste("rank ~", score_col_name)), data = merged_data, Hess = TRUE)
  ctable <- coef(summary(model))
  pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  model_summary <- data.frame(ctable, "p value" = pvals)
  
  return(list(
    spearman = spearman_result,
    model_summary = model_summary
  ))
}

# Combined model with all predictors
get_combined_clic_model <- function(df) {
  rank_data <- get_participant_id_and_rankings(df)
  
  cred <- get_clic_dimension_long(df, "Cred", "cred_score")
  like <- get_clic_dimension_long(df, "Like", "like_score")
  info <- get_clic_dimension_long(df, "Info", "info_score")
  clar <- get_clic_dimension_long(df, "Clar", "clar_score")
  
  merged_all <- rank_data %>%
    left_join(cred, by = c("participant_id", "design")) %>%
    left_join(like, by = c("participant_id", "design")) %>%
    left_join(info, by = c("participant_id", "design")) %>%
    left_join(clar, by = c("participant_id", "design"))
  
  merged_all$rank <- factor(merged_all$rank, levels = 4:1, ordered = TRUE)
  
  model <- MASS::polr(rank ~ cred_score + like_score + info_score + clar_score, data = merged_all, Hess = TRUE)
  ctable <- coef(summary(model))
  pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  model_summary <- data.frame(ctable, "p value" = pvals)
  
  return(model_summary)
}


