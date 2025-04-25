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



print_ranking_dist_plot <- function(df) {
  # create a dataset
  specie <- c(rep("rank 1" , 4) , rep("rank 2" , 4) , rep("rank 3" , 4) , rep("rank 4" , 4) )
  condition <- rep(c("insta" , "desc" , "warn", "draw") , 4)
  value <- c(
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
  data <- data.frame(specie,condition,value)
  
  # Stacked + percent
  plot <- ggplot(data, aes(fill=condition, y=value, x=specie)) + 
    geom_bar(position = "stack", stat = "identity") +  # Switch to "stack" for raw counts
    geom_text(aes(label = value), position = position_stack(vjust = 0.5))  # Add labels to each part of the bar
  
  print(plot)
}


