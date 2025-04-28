# Explnation: https://chatgpt.com/share/680e99e7-75c0-8000-90a1-3f8baec24876

library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(afex)
library(ggsignif)
library(ARTool)
library(PlackettLuce)

# Load your cleaned data
data <- get("cleaned_valid", envir = .GlobalEnv)

# Reshape the data into long format
data_long <- data %>%
  pivot_longer(cols = c("baselineClic", "descClic", "warnClic", "drawingClic"),  # select columns starting with 'clic'
               names_to = "prototype",      # name for prototype column
               values_to = "score")        # name for the score column

# Perform Shapiro-Wilk normality test for each prototype
shapiro_test_results <- data_long %>%
  group_by(prototype) %>%
  summarise(shapiro_p_value = shapiro.test(score)$p.value)

# Print Shapiro-Wilk test results
print(shapiro_test_results)

# Visualizations: Histograms and Q-Q plots
histogram <- ggplot(data_long, aes(x = score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~prototype) +
  theme_minimal() +
  labs(title = "Histogram of Scores by Prototype", x = "Score", y = "Frequency")

qqplot <- ggplot(data_long, aes(sample = score)) +
  stat_qq() + 
  stat_qq_line() +
  facet_wrap(~prototype) +
  theme_minimal() +
  labs(title = "Q-Q Plot by Prototype", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Print visualizations
print(histogram)
print(qqplot)

# Levene's Test for Homogeneity of Variance
# Make sure 'prototype' is a factor
data_long$prototype <- as.factor(data_long$prototype)
data_long$GROUP <- as.factor(data_long$GROUP)

levene_results <- leveneTest(score ~ prototype * GROUP, data = data_long)
print(levene_results)

# Repeated Measures ANOVA using the afex package (to account for correlated data)
# Define a repeated-measures model
anova_results <- aov_ez(id = "id",
                        dv = "score", 
                        within = "prototype", 
                        between = "GROUP", 
                        data = data_long)

# Print ANOVA results
print(anova_results)


#######################################################################

# Perform pairwise t-tests (Post-hoc test) between prototypes
pairwise_results <- pairwise.t.test(data_long$score, data_long$prototype, p.adjust.method = "bonferroni")

# Print pairwise t-test results
print(pairwise_results)

# Perform pairwise comparisons and visualize using ggplot2


# Create pairwise comparison plot
pairwise_plot <- ggplot(data_long, aes(x = prototype, y = score, fill = prototype)) +
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

# Print pairwise plot with significance annotations
print(pairwise_plot)

#######################################################################

rank_long <- data %>%
  pivot_longer(cols = c("ranking.1.", "ranking.2.", "ranking.3.", "ranking.4."),
               names_to = "rank_position",
               values_to = "prototype") %>%
  mutate(rank = as.numeric(gsub("ranking\\.", "", rank_position))) %>%
  select(id, GROUP, prototype, rank)

# Make sure GROUP and prototype are factors
rank_long$GROUP <- as.factor(rank_long$GROUP)
rank_long$prototype <- as.factor(rank_long$prototype)

# Wide format for Friedman test
rank_wide <- rank_long %>%
  pivot_wider(names_from = prototype, values_from = rank)

# Perform the Friedman test
friedman_result <- friedman.test(rank ~ prototype | id, data = rank_long)

# Print the result
print(friedman_result)

# ART model
art_model <- art(rank ~ prototype * GROUP + (1|id), data = rank_long)

# ANOVA on aligned ranks
anova_results <- anova(art_model)
print(anova_results)

wilcoxon_result <- pairwise.wilcox.test(rank_long$rank, rank_long$prototype, paired = TRUE, p.adjust.method = "bonferroni")

print(wilcoxon_result)

  boxplot <- ggplot(rank_long, aes(x = prototype, y = rank, fill = prototype)) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    theme_minimal() +
    labs(title = "Prototype Rankings", x = "Prototype", y = "Rank (lower is better)") +
    scale_y_reverse()


print(boxplot)

# Explicitly ensure 'insta' is the baseline
rank_long$prototype <- factor(rank_long$prototype, levels = c("insta", "desc", "warn", "draw"))

# Now fit the Plackett-Luce model
rank_matrix <- rank_long %>%
  pivot_wider(names_from = prototype, values_from = rank) %>%
  select(-id, -GROUP)  # Remove id and GROUP columns (not needed for PlackettLuce)

# Fit the Plackett-Luce model
pl_model <- PlackettLuce(rank_matrix)

# Print model results
pl_results <- summary(pl_model)
print(pl_results)

# Extract coefficients
coefficients <- coef(pl_model)

# Exponentiate the coefficients to get odds ratios
odds_ratios <- exp(coefficients)

# Normalize the odds ratios to sum to 1
normalized_weights <- odds_ratios / sum(odds_ratios)

# Print the normalized weights
print(normalized_weights)

# Prepare the data for plotting
coefficients_df <- data.frame(prototype = names(normalized_weights), weight = normalized_weights)

# Plot the normalized weights
coefficient_plot <- ggplot(coefficients_df, aes(x = prototype, y = weight)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Prototype Preference Weights", x = "Prototype", y = "Normalized Weight") +
  coord_flip()  # To flip the axes if you want a horizontal bar plot

# Print the plot
print(coefficient_plot)



