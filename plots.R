library(ggplot2)
library(tidyr)
library(dplyr)


print_group_dist_plot <- function(df) {
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
  print(plot)
}

print_gender_dist_plot <- function(df) {
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
  print(plot)
}


print_age_dist_plot <- function(df) {
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
  print(plot)
}

print_education_level_dist_plot <- function(df) {
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
  print(plot)
}


print_instagram_usage_plot <- function(df) {
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
  
  print(plot)
}



print_ers_box_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(sens, arou, pers),
                 names_to = "score_type",
                 values_to = "score_value")
  plot <- ggplot(df_long, aes(x = score_type, y = score_value)) +
    geom_boxplot(fill = "skyblue") +
    theme_minimal() +
    labs(title = "Distribution of Scores",
         x = "Score Type",
         y = "Score Value")
  
  print(plot)
}

print_clar_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineClar, descClar, warnClar, drawingClar),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Clarification Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  print(plot)
}

print_like_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineLike, descLike, warnLike, drawingLike),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Clarification Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  print(plot)
}

print_info_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineInfo, descInfo, warnInfo, drawingInfo),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Clarification Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  print(plot)
}

print_cred_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineCred, descCred, warnCred, drawingCred),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Clarification Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  print(plot)
}

print_clic_density_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(baselineClic, descClic, warnClic, drawingClic),
                 names_to = "score_type",
                 values_to = "score_value")
  
  plot <- ggplot(df_long, aes(x = score_value, fill = score_type)) +
    geom_density(alpha = 0.4) +
    theme_minimal() +
    labs(x = "Score", y = "Density", title = "Density Plot of Clarification Scores by Type") +
    scale_fill_brewer(palette = "Pastel1")
  
  print(plot)
}

print_ranking_dist_plot <- function(df) {
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
  
  print(plot)
}


