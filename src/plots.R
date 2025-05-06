library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

source("src/stats.R")

get_group_dist_plot <- function(df) {
  group_counts <- df %>%
    group_by(GROUP) %>%
    summarise(count = n())
  
  plot <- ggplot(group_counts, aes(x = factor(GROUP), y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Number of Participants in Each Group", 
         x = "Group", 
         y = "Number of Participants") +
    theme_minimal()
  
  return(plot)
}

get_gender_dist_plot <- function(df) {
  gender_counts <- df %>%
    group_by(gender) %>%
    summarise(count = n())
  
  plot <- ggplot(gender_counts, aes(x = factor(gender), y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Gender Distribution of Participants", 
         x = "Gender", 
         y = "Amount") +
    theme_minimal()
  return(plot)
}

get_age_dist_plot <- function(df) {
  age_counts <- df %>%
    group_by(age) %>%
    summarise(count = n())
  
  plot <- ggplot(age_counts, aes(x = age, y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Age Distribution of Participants", 
         x = "Age", 
         y = "Number of Participants") +
    theme_minimal()
  return(plot)
}

get_education_level_dist_plot <- function(df) {
  education_counts <- df %>%
    group_by(education) %>%
    summarise(count = n())
  
  plot <- ggplot(education_counts, aes(x = education, y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Education Level Distribution of Participants", 
         x = "Education Level", 
         y = "Number of Participants") +
    theme_minimal()
  return(plot)
}

get_instagram_usage_plot <- function(df) {
  activity_summary <- df %>%
    select(all_of(c(
      "primaryInstaUsage.photos.",
      "primaryInstaUsage.stories.",
      "primaryInstaUsage.friends.",
      "primaryInstaUsage.celebs.", 
      "primaryInstaUsage.trends.",
      "primaryInstaUsage.brands.",
      "primaryInstaUsage.communities.",
      "primaryInstaUsage.streams.",
      "primaryInstaUsage.messaging.",
      "primaryInstaUsage.news.",
      "primaryInstaUsage.other."
    ))) %>%
    mutate(across(everything(), ~ ifelse(. == "Y", 1, 0))) %>%  # recode Y to 1, NA to 0
    summarise(across(everything(), sum)) %>%
    pivot_longer(cols = everything(), names_to = "activity", values_to = "count") %>%
    mutate(
      percent = count / sum(count) * 100,
      label = paste0(round(percent, 1), "%")
    )  # <-- calculate percentages and create labels
  
  plot <- ggplot(activity_summary, aes(x = "", y = count, fill = activity)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    theme_void() +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 4) +  # <-- add percent labels
    labs(title = "Instagram Activities by Respondents") +
    theme(legend.title = element_blank())
  
  return(plot)
}

get_ers_box_plot <- function(df) {
  df_long <- df %>%
    mutate(
      sens = sens / 40,
      arou = arou / 28,
      pers = pers / 16,
      ers = ers / 82
    ) %>%
    pivot_longer(cols = c(sens, arou, pers, ers),
                 names_to = "score_type",
                 values_to = "score_value") %>%
    mutate(score_type = factor(score_type, levels = c("sens", "arou", "pers", "ers")))
  
  plot <- ggplot(df_long, aes(x = score_type, y = score_value)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Distribution of Scores",
         x = "Score Type",
         y = "Score Value")
  
  return(plot)
}

get_clar_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineClar, descClar, warnClar, drawingClar),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Clarification Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  return(plot)
}

get_like_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineLike, descLike, warnLike, drawingLike),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Likability Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  return(plot)
}

get_info_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineInfo, descInfo, warnInfo, drawingInfo),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Informational Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  return(plot)
}

get_cred_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineCred, descCred, warnCred, drawingCred),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Credibility Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  return(plot)
}

get_clic_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineClic, descClic, warnClic, drawingClic),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Overall Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  return(plot)
}

get_ranking_dist_plot <- function(df) {
  # create a dataset
  rank <- c(rep("rank 1" , 4) , rep("rank 2" , 4) , rep("rank 3" , 4) , rep("rank 4" , 4) )
  condition <- rep(c("insta" , "desc" , "warn", "draw") , 4)
  votes <- c(
    table(df$ranking.1)["insta"],
    table(df$ranking.1)["desc"],
    table(df$ranking.1)["warn"],
    table(df$ranking.1)["draw"],
    table(df$ranking.2)["insta"],
    table(df$ranking.2)["desc"],
    table(df$ranking.2)["warn"],
    table(df$ranking.2)["draw"],
    table(df$ranking.3)["insta"],
    table(df$ranking.3)["desc"],
    table(df$ranking.3)["warn"],
    table(df$ranking.3)["draw"],
    table(df$ranking.4)["insta"],
    table(df$ranking.4)["desc"],
    table(df$ranking.4)["warn"],
    table(df$ranking.4)["draw"]
  )
  data <- data.frame(rank,condition,votes)
  
  # Stacked + percent
  plot <- ggplot(data, aes(fill=condition, y=votes, x=rank)) + 
    geom_bar(position = "stack", stat = "identity") +  # Switch to "stack" for raw counts
    geom_text(aes(label = votes), position = position_stack(vjust = 0.5))  # Add labels to each part of the bar
  
  return(plot)
}

get_clic_histogram_plot <- function(df) {
  data_long <- get_clic_long_data(df)
  
  plot <- histogram <- ggplot(data_long, aes(x = score)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    facet_wrap(~prototype) +
    theme_minimal() +
    labs(title = "Histogram of Scores by Prototype", x = "Score", y = "Frequency")
  
  return(plot)
}

get_clic_qq_plot <- function(df) {
  data_long <- get_clic_long_data(df)
  
  plot <- qqplot <- ggplot(data_long, aes(sample = score)) +
    stat_qq() + 
    stat_qq_line() +
    facet_wrap(~prototype) +
    theme_minimal() +
    labs(title = "Q-Q Plot by Prototype", x = "Theoretical Quantiles", y = "Sample Quantiles")
  return(plot)
}  
  
get_pairwise_rank_plot <- function(df) {
  data_long <- get_clic_long_data(df)
  
  plot <- ggplot(data_long, aes(x = prototype, y = score, fill = prototype)) +
    geom_boxplot(alpha = 0.6) +
    geom_signif(comparisons = list(c("descClic", "baselineClic"), 
                                   c("drawingClic", "baselineClic"),
                                   c("drawingClic", "descClic"),
                                   c("warnClic", "baselineClic"),
                                   c("warnClic", "descClic"),
                                   c("warnClic", "drawingClic")),
                map_signif_level = TRUE) +  # Adds significance markers
    labs(title = "Pairwise Comparisons of Prototypes", x = "Prototype", y = "Score") +
    theme_minimal() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set3")
  return(plot)
}

get_rank_coefficient_plot <- function(df) {
  normalized_weights <- get_rank_placketluce_coef_results(df)
  coefficients_df <- data.frame(prototype = names(normalized_weights), weight = normalized_weights)
  
  plot <- ggplot(coefficients_df, aes(x = prototype, y = weight)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme_minimal() +
    labs(title = "Prototype Preference Weights", x = "Prototype", y = "Normalized Weight") +
    coord_flip()  # To flip the axes if you want a horizontal bar plot
  return(plot)
}
  
