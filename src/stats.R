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
library(emmeans)

get_clic_long_data <- function(df) {
  data_long <- df %>%
    pivot_longer(cols = c("baselineClic", "descClic", "warnClic", "drawingClic"),  # select columns starting with 'clic'
                 names_to = "prototype",
                 values_to = "score") %>%
    mutate(prototype = factor(prototype,
                              levels = c("baselineClic", "descClic", "warnClic", "drawingClic")))
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

get_clic_descriptive_stats_results <- function(df) {
  data_long <- get_clic_long_data(df)
  
  prototype_descriptives <- data_long %>%
    group_by(prototype) %>%
    summarise(
      mean = round(mean(score, na.rm = TRUE), 2),
      sd = round(sd(score, na.rm = TRUE), 2),
      min = round(min(score, na.rm = TRUE), 2),
      max = round(max(score, na.rm = TRUE), 2)
    )
  
  return(prototype_descriptives)
}

get_clic_tukey_results <- function(df) {
  anova_results <- get_clic_repeated_measures_anova_results(df)
  
  emm <- emmeans(anova_results, ~ prototype)
  pairwise_results <- contrast(emm, method = "pairwise", adjust = "tukey")
  
  return(pairwise_results)
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
    mutate(rank = as.numeric(gsub("ranking\\.", "", rank_position)))
  return(rank_long)
}

get_rank_matrix_data <- function(df) {
  rank_long <- get_rank_long_data(df) %>%
    dplyr::select(id, p_seq, prototype, rank)
  
  rank_matrix <- rank_long %>%
    tidyr::pivot_wider(names_from = prototype, values_from = rank) %>%
    dplyr::select(insta, desc, warn, draw)
  
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
  view(rank_matrix)
  pl_model <- PlackettLuce(rank_matrix)
  normalized_weights <- coef(pl_model, log=FALSE)

  return(normalized_weights)
}

################################################

get_rank_clic_polr_results <- function(df) {
  rank_data_long <- get_rank_long_data(df) %>%
    mutate(
      clar_score = case_when(
        prototype == "insta" ~ baselineClar,
        prototype == "desc" ~ descClar,
        prototype == "draw" ~ drawingClar,
        prototype == "warn" ~ warnClar,
        TRUE ~ NA_real_
      ),
      like_score = case_when(
        prototype == "insta" ~ baselineLike,
        prototype == "desc" ~ descLike,
        prototype == "draw" ~ drawingLike,
        prototype == "warn" ~ warnLike,
        TRUE ~ NA_real_
      ),
      info_score = case_when(
        prototype == "insta" ~ baselineInfo,
        prototype == "desc" ~ descInfo,
        prototype == "draw" ~ drawingInfo,
        prototype == "warn" ~ warnInfo,
        TRUE ~ NA_real_
      ),
      cred_score = case_when(
        prototype == "insta" ~ baselineCred,
        prototype == "desc" ~ descCred,
        prototype == "draw" ~ drawingCred,
        prototype == "warn" ~ warnCred,
        TRUE ~ NA_real_
      ),
      rank = factor(rank, levels = c(4, 3, 2, 1), ordered = TRUE)
    ) %>%
    dplyr::select(prototype, rank, clar_score, like_score, info_score, cred_score)
  
  # Fit proportional odds logistic regression
  model <- MASS::polr(rank ~ cred_score + info_score, data = rank_data_long, Hess = TRUE)
  
  # Extract coefficients and compute p-values
  ctable <- coef(summary(model))
  pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  model_summary <- data.frame(ctable, "p value" = pvals)
  
  return(model_summary)
}


get_sens_info_data <- function(df) {
  data <- df %>%
    mutate(infoscore_rank1 = case_when(
      ranking.1. == "insta" ~ baselineInfo,
      ranking.1. == "desc" ~ descInfo,
      ranking.1. == "draw" ~ drawingInfo,
      ranking.1. == "warn" ~ warnInfo,
      TRUE ~ NA_real_  # in case of unexpected values
    ))
  return(data)
}


get_spearman_sens_info_results <- function(df) {
  data <- get_sens_info_data(df)
  result <- cor.test(data$sens, data$infoscore_rank1, method = "spearman")
  
  return(result)
}


get_better_scs_results <- function(df) {
  # Remove NA values before calculation
  mean_val <- mean(df$`betterSCS.SQ001.`, na.rm = TRUE)
  sd_val <- sd(df$`betterSCS.SQ001.`, na.rm = TRUE)
  
  results <- list(mean = mean_val, sd = sd_val)
  return(results)
}

get_ers_rank_1_means_results <- function(df) {
  # Each participant has one ERS score — associate it with their first-ranked prototype
  summary_stats <- df %>%
    group_by(ranking.1.) %>%
    summarise(
      Mean = round(mean(ers, na.rm = TRUE), 2),
      SD = round(sd(ers, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    rename(rank_1_prototype = ranking.1.)
  
  return(summary_stats)
}


