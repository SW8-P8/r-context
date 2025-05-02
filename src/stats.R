library(ggplot2)
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
  
  data_long$prototype <- as.factor(data_long$prototype)
  data_long$GROUP <- as.factor(data_long$GROUP)
  
  levene_results <- leveneTest(score ~ prototype * GROUP, data = data_long)
  return(levene_results)
}

get_clic_repeated_measures_anova_results <- function(df) {
  data_long <- get_clic_long_data(df)
  
  data_long$GROUP <- as.factor(data_long$GROUP)
  data_long$prototype <- as.factor(data_long$prototype)
  data_long$id <- as.factor(data_long$id)
  data_long$gender <- as.factor(data_long$gender)
  
  anova_results <- aov_ez(id = "id", 
                         dv = "score", 
                         data = data_long, 
                         within = "prototype", 
                         between = "GROUP")
  
  return(anova_results)
}

get_clic_pairwise_prototype_t_test_results <- function(df) {
  data_long <- get_clic_long_data(df)
  pairwise_results <- pairwise.t.test(data_long$score, data_long$prototype, p.adjust.method = "bonferroni")
  return(pairwise_results)
}

get_clic_pairwise_group_t_test_results <- function(df) {
  data_long <- get_clic_long_data(df)
  pairwise_results <- pairwise.t.test(data_long$score, data_long$GROUP, p.adjust.method = "bonferroni")
  return(pairwise_results)
}

###########################################z############################

get_rank_long_data <- function(df) {
  rank_long <- df %>%
    pivot_longer(cols = c("ranking.1.", "ranking.2.", "ranking.3.", "ranking.4."),
                 names_to = "rank_position",
                 values_to = "prototype") %>%
    mutate(rank = as.numeric(gsub("ranking\\.", "", rank_position))) %>%
    select(id, GROUP, prototype, rank)
  return(rank_long)
}

get_rank_matrix_data <- function(df) {
  rank_long <- get_rank_long_data(df)
  rank_matrix <- rank_long %>%
    pivot_wider(names_from = prototype, values_from = rank) %>%
    select(-id, -GROUP)
  return(rank_matrix)
}

get_rank_friedman_results <- function(df) {
  rank_long <- get_rank_long_data(df)
  
  rank_long$GROUP <- as.factor(rank_long$GROUP)
  rank_long$prototype <- as.factor(rank_long$prototype)
  
  friedman_result <- friedman.test(rank ~ prototype | id, data = rank_long)
}

get_rank_anova_results <- function(df) {
  rank_long <- get_rank_long_data(df)
  
  rank_long$GROUP <- as.factor(rank_long$GROUP)
  rank_long$prototype <- as.factor(rank_long$prototype)
  
  art_model <- art(rank ~ prototype * GROUP + (1|id), data = rank_long)
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


