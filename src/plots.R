library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(tidyselect)
library(shadowtext)

source("src/stats.R")

get_p_seq_dist_plot <- function(df) {
  p_seq_counts <- df %>%
    group_by(p_seq) %>%
    summarise(count = n())
  
  plot <- ggplot(p_seq_counts, aes(x = factor(p_seq), y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = count), vjust = -0.5) +
    labs(title = "Number of Participants in Each p-seq", 
         x = "p-seq", 
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
    dplyr::select(all_of(c(
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
  
  # Define readable activity labels
  activity_labels <- c(
    "primaryInstaUsage.photos." = "Posting Photos",
    "primaryInstaUsage.stories." = "Viewing Stories",
    "primaryInstaUsage.friends." = "Following Friends",
    "primaryInstaUsage.celebs." = "Following Celebrities",
    "primaryInstaUsage.trends." = "Checking Trends",
    "primaryInstaUsage.brands." = "Following Brands",
    "primaryInstaUsage.communities." = "Engaging with Communities",
    "primaryInstaUsage.streams." = "Watching Livestreams",
    "primaryInstaUsage.messaging." = "Messaging",
    "primaryInstaUsage.news." = "Reading News",
    "primaryInstaUsage.other." = "Other"
  )
  
  plot <- ggplot(activity_summary, aes(x = "", y = count, fill = activity)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    theme_void() +
    geom_shadowtext(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "white",
      bg.color = "black",  # shadow/border color
      size = 4
    ) +  # <-- add percent labels
    theme(legend.title = element_blank()) +
    scale_fill_manual(
      values = RColorBrewer::brewer.pal(11, "Paired"),  # Optional: reuse Set3 for visual variety
      labels = activity_labels
    ) +
    theme(legend.title = element_blank())  # Removes the title from the legend
  
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
    mutate(score_type = factor(score_type,
                               levels = c("sens", "arou", "pers", "ers"),
                               labels = c("Sensitivity", "Arousal", "Persistence", "Total ERS")))
  
  plot <- ggplot(df_long, aes(x = score_type, y = score_value, fill = score_type)) +
    geom_boxplot(alpha = 0.8, outlier.shape = NA, fill="skyblue") +
    geom_jitter(color = "black", size = 0.4, alpha = 0.9, width = 0.15) +
    labs(x = "", y = "Normalized ERS Score") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none"
    ) 
  return(plot)
}

get_clar_density_plot <- function(df) {
  library(dplyr)
  library(ggplot2)
  library(tidyr)

  
  df_long <- df %>%
    pivot_longer(cols = c(baselineClar, descClar, warnClar, drawingClar),
                 names_to = "score_type",
                 values_to = "score_value") %>%
    mutate(score_type = factor(score_type,
                            levels = c("baselineClar", "descClar", "warnClar", "drawingClar")))
  
  label_map <- c(
    "baselineClar" = "Baseline",
    "descClar" = "Content Description",
    "warnClar" = "Trigger Warnings",
    "drawingClar" = "Drawing Filter"
  )
  
  means_df <- df_long %>%
    group_by(score_type) %>%
    summarize(mean_score = mean(score_value, na.rm = TRUE), .groups = "drop")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = means_df,
               aes(xintercept = mean_score, color = score_type),
               linetype = "dashed", size = 1) +
    geom_vline(xintercept = 5.33, linetype = "dashed", color = "black", size = 1) +
    theme_minimal() +
    labs(fill = "", color = "", x = "Web-CLIC Clarification Score", y = "Density") +
    scale_fill_brewer(palette = "Set1", labels = label_map) +
    scale_color_brewer(palette = "Set1", labels = label_map)
  
  return(plot)
}


get_like_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineLike, descLike, warnLike, drawingLike),
                 names_to = "score_type",
                 values_to = "score_value") %>%
    mutate(score_type = factor(score_type,
                              levels = c("baselineLike", "descLike", "warnLike", "drawingLike")))
  
  label_map <- c(
    "baselineLike" = "Baseline",
    "descLike" = "Content Description",
    "warnLike" = "Trigger Warnings",
    "drawingLike" = "Drawing Filter"
  )
  
  means_df <- df_long %>%
    group_by(score_type) %>%
    summarize(mean_score = mean(score_value, na.rm = TRUE),sd_score = sd(score_value, na.rm = TRUE), .groups = "drop")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = means_df,
               aes(xintercept = mean_score, color = score_type),
               linetype = "dashed", size = 1) +
    geom_vline(xintercept = 4.00, linetype = "dashed", color = "black", size = 1) +
    theme_minimal() +
    labs(fill = "", color = "", x = "Web-CLIC Likability Score", y = "Density") +
    scale_fill_brewer(palette = "Set1", labels = label_map) +
    scale_color_brewer(palette = "Set1", labels = label_map)
  return(plot)
}

get_info_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineInfo, descInfo, warnInfo, drawingInfo),
                 names_to = "score_type",
                 values_to = "score_value") %>%
    mutate(score_type = factor(score_type,
                              levels = c("baselineInfo", "descInfo", "warnInfo", "drawingInfo")))
  
  
  label_map <- c(
    "baselineInfo" = "Baseline",
    "descInfo" = "Content Description",
    "warnInfo" = "Trigger Warnings",
    "drawingInfo" = "Drawing Filter"
  )
  
  means_df <- df_long %>%
    group_by(score_type) %>%
    summarize(mean_score = mean(score_value, na.rm = TRUE), .groups = "drop")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = means_df,
               aes(xintercept = mean_score, color = score_type),
               linetype = "dashed", size = 1) +
    geom_vline(xintercept = 5.00, linetype = "dashed", color = "black", size = 1) +
    theme_minimal() +
    labs(fill = "", color = "", x = "Web-CLIC Informativeness Score", y = "Density") +
    scale_fill_brewer(palette = "Set1", labels = label_map) +
    scale_color_brewer(palette = "Set1", labels = label_map)
  
  return(plot)
}

get_cred_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineCred, descCred, warnCred, drawingCred),
                 names_to = "score_type",
                 values_to = "score_value") %>%
    mutate(score_type = factor(score_type,
                              levels = c("baselineCred", "descCred", "warnCred", "drawingCred")))
  
  label_map <- c(
    "baselineCred" = "Baseline",
    "descCred" = "Content Description",
    "warnCred" = "Trigger Warnings",
    "drawingCred" = "Drawing Filter"
  )
  
  means_df <- df_long %>%
    group_by(score_type) %>%
    summarize(mean_score = mean(score_value, na.rm = TRUE), .groups = "drop")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.3) +
    theme_minimal() +
    geom_vline(data = means_df,
               aes(xintercept = mean_score, color = score_type),
               linetype = "dashed", size = 1) +
    geom_vline(xintercept = 4.67, linetype = "dashed", color = "black", size = 1) +
    labs(fill = "", color = "", x = "Web-CLIC Credibility Score", y = "Density") +
    scale_fill_brewer(palette = "Set1", labels = label_map) +
    scale_color_brewer(palette = "Set1", labels = label_map)
  
  return(plot)
}

get_clic_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineClic, descClic, warnClic, drawingClic),
                 names_to = "score_type",
                 values_to = "score_value") %>%
    mutate(score_type = factor(score_type,
                               levels = c("baselineClic", "descClic", "warnClic", "drawingClic")))
  
  label_map <- c(
    "baselineClic" = "Instagram's Default",
    "descClic" = "Content Description",
    "warnClic" = "Trigger Warnings",
    "drawingClic" = "Drawing Filter"
  )
  
  means_df <- df_long %>%
    group_by(score_type) %>%
    summarize(mean_score = mean(score_value, na.rm = TRUE), .groups = "drop")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = means_df,
               aes(xintercept = mean_score, color = score_type),
               linetype = "dashed", size = 1) +
    geom_vline(xintercept = 4.58, linetype = "dashed", color = "black", size = 1) +
    labs(fill = "", color = "", x = "Web-CLIC Score", y = "Density") +
    scale_fill_brewer(palette = "Set1", labels = label_map) +
    scale_color_brewer(palette = "Set1", labels = label_map) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.box.background = element_blank()
    )
  
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
  
  # Set desired fill order
  data$condition <- factor(data$condition, levels = c("insta", "desc", "warn", "draw"))
  
  # Stacked + percent
  plot <- ggplot(data, aes(fill=condition, y=votes, x=rank)) + 
    geom_bar(position = "stack", stat = "identity", alpha = 0.8) +  # Switch to "stack" for raw counts
    geom_text(aes(label = votes), position = position_stack(vjust = 0.5), color = "white") +  # Add labels to each part of the bar
    theme_minimal() +
    scale_fill_brewer(
      palette = "Set1",
      labels = c(
        "insta" = "Instagram's Default",
        "desc" = "Content Description",
        "warn" = "Trigger Warnings",
        "draw" = "Drawing Filter"
      )
    ) +
    scale_x_discrete(labels = c(
      "rank 1" = "1st Choice",
      "rank 2" = "2nd Choice",
      "rank 3" = "3rd Choice",
      "rank 4" = "4th Choice"
    )) +
    labs(fill = "", x = "", y = "Votes") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    )
  return(plot)
}

get_clic_histogram_plot <- function(df) {
  data_long <- get_clic_long_data(df)
  
  # Define label replacements
  prototype_labels <- c(
    "baselineClic" = "Baseline",
    "descClic" = "Content Description",
    "warnClic" = "Trigger Warnings",
    "drawingClic" = "Drawing Filter"
  )
  
  plot <- histogram <- ggplot(data_long, aes(x = score, fill = prototype)) +
    geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
    facet_wrap(~prototype, labeller = as_labeller(prototype_labels)) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1") +
    labs(x = "Web-CLIC Score", y = "Frequency") +
    theme(legend.position = "none")
  return(plot)
}

get_clic_qq_plot <- function(df) {
  data_long <- get_clic_long_data(df)
  
  # Define label replacements
  prototype_labels <- c(
    "baselineClic" = "Baseline",
    "descClic" = "Content Description",
    "warnClic" = "Trigger Warnings",
    "drawingClic" = "Drawing Filter"
  )
  
  plot <- qqplot <- ggplot(data_long, aes(sample = score, color = prototype)) +
    stat_qq() + 
    stat_qq_line() +
    facet_wrap(~prototype, labeller = as_labeller(prototype_labels)) +
    theme_minimal() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "none")
  return(plot)
} 
  
get_clic_pairwise_prototype_plot <- function(df) {
  data_long <- get_clic_long_data(df)
  
  plot <- ggplot(data_long, aes(x = prototype, y = score, fill = prototype)) +
    geom_boxplot(alpha = 0.8) +
    geom_signif(comparisons = list(c("descClic", "baselineClic"), 
                                   c("drawingClic", "baselineClic"),
                                   c("drawingClic", "descClic"),
                                   c("warnClic", "baselineClic"),
                                   c("warnClic", "descClic"),
                                   c("warnClic", "drawingClic")),
                map_signif_level = TRUE) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "white", size = 0.3) +  # Mean line per box
    labs(x = "", y = "Web-CLIC Score") +
    theme_minimal() +
    geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
    geom_hline(yintercept = 4.58, linetype = "dashed", color = "black", size = 1) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set1") + 
    scale_x_discrete(labels = c("baselineClic" = "Baseline",
                                "descClic" = "Content Description",
                                "warnClic" = "Trigger Warnings",
                                "drawingClic" = "Drawing Filter"))
  return(plot)
}


get_rank_coefficient_plot <- function(df) {
  normalized_weights <- get_rank_placketluce_coef_results(df)
  coefficients_df <- data.frame(prototype = names(normalized_weights), weight = normalized_weights)
  
  
  # Set desired order of prototypes
  coefficients_df$prototype <- factor(coefficients_df$prototype,
                                      levels = c("insta", "desc","warn", "draw"))
  
  plot <- ggplot(coefficients_df, aes(x = prototype, y = weight, fill = prototype)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    theme_minimal() +
    labs(x = "", y = "Normalized Weight (Ranking)") +
    coord_flip() +  # To flip the axes if you want a horizontal bar plot
    scale_fill_brewer(palette = "Set1") +
    scale_x_discrete(labels = c("insta" = "Baseline",
                                "desc" = "Content Description",
                                "warn" = "Trigger Warnings",
                                "draw" = "Drawing Filter")) +
    theme(legend.position = "none")
  return(plot)
}

get_correlation_sens_info_plot <- function(df) {
  data <- get_sens_info_data(df)
  
  plot <- ggplot(data, aes(y = infoscore_rank1, x = sens)) +
    geom_point() +
    geom_smooth(method = lm, color = "red", fill = "#69b3a2", se = TRUE) +
    labs(
      x = "Participant Sensitivity Score",
      y = "Informativeness Score of 1st Choice Prototype"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(plot)
}

get_combined_clic_density_plot <- function(df) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Reshape and label data
  df_long <- df %>%
    pivot_longer(
      cols = c(baselineClar, descClar, warnClar, drawingClar,
               baselineLike, descLike, warnLike, drawingLike,
               baselineInfo, descInfo, warnInfo, drawingInfo,
               baselineCred, descCred, warnCred, drawingCred),
      names_to = c("prototype", "dimension"),
      names_pattern = "(baseline|desc|warn|drawing)(Clar|Like|Info|Cred)",
      values_to = "score"
    ) %>%
    mutate(
      prototype = factor(prototype, levels = c("baseline", "desc", "warn", "drawing")),
      dimension = case_when(
        dimension == "Clar" ~ "Clarification",
        dimension == "Like" ~ "Likability",
        dimension == "Info" ~ "Informativeness",
        dimension == "Cred" ~ "Credibility"
      ),
      prototype_label = case_when(
        prototype == "baseline" ~ "Baseline",
        prototype == "desc" ~ "Content Description",
        prototype == "warn" ~ "Trigger Warnings",
        prototype == "drawing" ~ "Drawing Filter"
      ),
      prototype_label = factor(
        prototype_label,
        levels = c("Baseline", "Content Description", "Trigger Warnings", "Drawing Filter")
      )
    )
  
  # Compute means
  means_df <- df_long %>%
    group_by(dimension, prototype_label) %>%
    summarize(mean_score = mean(score, na.rm = TRUE), .groups = "drop")
  
  # Define benchmark lines
  benchmark_lines <- data.frame(
    dimension = c("Clarification", "Likability", "Informativeness", "Credibility"),
    benchmark = c(5.33, 4.00, 5.00, 4.67)
  )
  
  # Join benchmark
  df_long <- left_join(df_long, benchmark_lines, by = "dimension")
  
  # Plot
  plot <- ggplot(df_long, aes(x = score, fill = prototype_label)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = means_df,
               aes(xintercept = mean_score, color = prototype_label),
               linetype = "dashed", size = 0.5) +
    geom_vline(aes(xintercept = benchmark),
               linetype = "dashed", color = "black", size = 0.5) +
    facet_wrap(~dimension, scales = "free", ncol = 2) +
    theme_minimal() +
    labs(x = "Web-CLIC Sub-Scores", y = "Density", fill = "", color = "") +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    )
  
  return(plot)
}


  
