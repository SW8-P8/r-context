library(MASS)
library(effects)
library(ggplot2)
library(dplyr)
library(tidyr)

###################

# Correlation between credibility and ranking

# Step 1: Reshape Ranking Data
ranking_long <- cleaned_valid %>%
  dplyr::select(participant_id = id, ranking.1., ranking.2., ranking.3., ranking.4.) %>%
  pivot_longer(cols = starts_with("ranking."),
               names_to = "rank_position",
               values_to = "design") %>%
  mutate(rank = as.numeric(gsub("ranking\\.", "", rank_position))) %>%
  dplyr::select(participant_id, design, rank)

# Step 2: Reshape Credibility Data
cred_long <- cleaned_valid %>%
  dplyr::select(participant_id = id, 
                baselineCred, descCred, warnCred, drawingCred) %>%
  pivot_longer(cols = c(baselineCred, descCred, warnCred, drawingCred),
               names_to = "design_cred",
               values_to = "cred_score") %>%
  mutate(design = tolower(gsub("Cred", "", design_cred))) %>%
  dplyr::select(participant_id, design, cred_score)

cred_long$design <- gsub("drawing", "draw", cred_long$design)
cred_long$design <- gsub("baseline", "insta", cred_long$design)

# Step 3: Merge Ranking and Credibility Data
combined_cred_rank <- left_join(ranking_long, cred_long, by = c("participant_id", "design"))

# Step 4a: Perform Spearman Correlation (to examine relationship between rank and cred_score)
cor_test <- cor.test(combined_cred_rank$rank, combined_cred_rank$cred_score, method = "spearman")

# Step 4b: Perform Ordinal Logistic Regression (to examine the effect of credibility on ranking)
# Convert rank to an ordered factor
combined_cred_rank$rank <- factor(combined_cred_rank$rank, levels = 4:1, ordered = TRUE)

# Fit ordinal logistic regression model
model <- MASS::polr(rank ~ cred_score, data = combined_cred_rank, Hess = TRUE)

# Get p-values for the model
ctable <- coef(summary(model))
pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
print(data.frame(ctable, "p value" = pvals))


# Correlation between likeability and ranking

# Step 2: Reshape Credibility Data
like_long <- cleaned_valid %>%
  dplyr::select(participant_id = id, 
                baselineLike, descLike, warnLike, drawingLike) %>%
  pivot_longer(cols = c(baselineLike, descLike, warnLike, drawingLike),
               names_to = "design_like",
               values_to = "like_score") %>%
  mutate(design = tolower(gsub("Like", "", design_like))) %>%
  dplyr::select(participant_id, design, like_score)

like_long$design <- gsub("drawing", "draw", like_long$design)
like_long$design <- gsub("baseline", "insta", like_long$design)

# Step 3: Merge Ranking and Credibility Data
combined_like_rank <- left_join(ranking_long, like_long, by = c("participant_id", "design"))

# Step 4a: Perform Spearman Correlation (to examine relationship between rank and cred_score)
cor_test <- cor.test(combined_like_rank$rank, combined_like_rank$like_score, method = "spearman")

# Step 4b: Perform Ordinal Logistic Regression (to examine the effect of credibility on ranking)
# Convert rank to an ordered factor
combined_like_rank$rank <- factor(combined_like_rank$rank, levels = 4:1, ordered = TRUE)

# Fit ordinal logistic regression model
model_like <- MASS::polr(rank ~ like_score, data = combined_like_rank, Hess = TRUE)

# Get p-values for the model
ctable <- coef(summary(model_like))
pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
print(data.frame(ctable, "p value" = pvals))

# Correlation between informativeness and ranking

# Step 2: Reshape Credibility Data
info_long <- cleaned_valid %>%
  dplyr::select(participant_id = id, 
                baselineInfo, descInfo, warnInfo, drawingInfo) %>%
  pivot_longer(cols = c(baselineInfo, descInfo, warnInfo, drawingInfo),
               names_to = "design_info",
               values_to = "info_score") %>%
  mutate(design = tolower(gsub("Info", "", design_info))) %>%
  dplyr::select(participant_id, design, info_score)

info_long$design <- gsub("drawing", "draw", info_long$design)
info_long$design <- gsub("baseline", "insta", info_long$design)

# Step 3: Merge Ranking and Credibility Data
combined_info_rank <- left_join(ranking_long, info_long, by = c("participant_id", "design"))

# Step 4a: Perform Spearman Correlation (to examine relationship between rank and cred_score)
cor_test <- cor.test(combined_info_rank$rank, combined_info_rank$info_score, method = "spearman")

# Step 4b: Perform Ordinal Logistic Regression (to examine the effect of credibility on ranking)
# Convert rank to an ordered factor
combined_info_rank$rank <- factor(combined_info_rank$rank, levels = 4:1, ordered = TRUE)

# Fit ordinal logistic regression model
model_info <- MASS::polr(rank ~ info_score, data = combined_info_rank, Hess = TRUE)

# Get p-values for the model
ctable <- coef(summary(model_info))
pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
print(data.frame(ctable, "p value" = pvals))


# Correlation between clarity and ranking

# Step 2: Reshape Credibility Data
clar_long <- cleaned_valid %>%
  dplyr::select(participant_id = id, 
                baselineClar, descClar, warnClar, drawingClar) %>%
  pivot_longer(cols = c(baselineClar, descClar, warnClar, drawingClar),
               names_to = "design_clar",
               values_to = "clar_score") %>%
  mutate(design = tolower(gsub("Clar", "", design_clar))) %>%
  dplyr::select(participant_id, design, clar_score)

clar_long$design <- gsub("drawing", "draw", clar_long$design)
clar_long$design <- gsub("baseline", "insta", clar_long$design)

# Step 3: Merge Ranking and Credibility Data
combined_clar_rank <- left_join(ranking_long, clar_long, by = c("participant_id", "design"))

# Step 4a: Perform Spearman Correlation (to examine relationship between rank and cred_score)
cor_test <- cor.test(combined_clar_rank$rank, combined_clar_rank$clar_score, method = "spearman")

# Step 4b: Perform Ordinal Logistic Regression (to examine the effect of credibility on ranking)
# Convert rank to an ordered factor
combined_clar_rank$rank <- factor(combined_clar_rank$rank, levels = 4:1, ordered = TRUE)

# Fit ordinal logistic regression model
model_clar <- MASS::polr(rank ~ clar_score, data = combined_clar_rank, Hess = TRUE)

# Get p-values for the model
ctable <- coef(summary(model_clar))
pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
print(data.frame(ctable, "p value" = pvals))


# Combination of all 3
combined_clic_long <- ranking_long %>%
  left_join(cred_long, by = c("participant_id", "design")) %>%
  left_join(like_long, by = c("participant_id", "design")) %>%
  left_join(info_long, by = c("participant_id", "design")) %>%
  left_join(clar_long, by = c("participant_id", "design"))

combined_clic_long$rank <- factor(combined_clic_long$rank, levels = 4:1, ordered = TRUE)

model_clic <- MASS::polr(rank ~ cred_score + like_score + info_score + clar_score, data = combined_clic_long, Hess = TRUE)

ctable <- coef(summary(model_clic))
pvals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
print(data.frame(ctable, "p value" = pvals))

# In a combined ordinal regression model using the Web-CLIC attributes, 
# informativeness (β = 0.76, p < .001) and likeability (β = 0.28, p < .001) 
# significantly predicted higher design rankings. 
# Clarity was not a significant predictor (p = .17), and credibility showed a 
# weak negative association when controlling for other factors (β = –0.25, p = .023). 
# These results suggest that informativeness and likeability
# are the most influential components in determining user preference.


# Load necessary packages
library(gt)

# Create the data frame
results_df <- data.frame(
  Predictor = rownames(ctable),
  Estimate = round(ctable[, "Value"], 3),
  Std_Error = round(ctable[, "Std. Error"], 3),
  t_value = round(ctable[, "t value"], 3),
  p_value = ifelse(pvals < 0.0001, "< .0001", format(round(pvals, 3), nsmall = 3)),
  row.names = NULL
)

results_df <- results_df[!results_df$Predictor %in% c("4|3", "3|2", "2|1"), ]

results_df$Predictor <- recode(results_df$Predictor,
                               "cred_score" = "Credibility",
                               "like_score" = "Likeability",
                               "info_score" = "Information",
                               "clar_score" = "Clarity")

results_df %>%
  gt() %>%
  tab_header(
    title = md("**Web-CLIC Measurement Impact on Ranking**"),
    subtitle = md("*Ordinal regression coefficients controlling for other variables*")
  ) %>%
  cols_label(
    Estimate = "Impact",
    Std_Error = "Std. Error",
    t_value = "t-value",
    p_value = "p-value"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(
    everything() ~ px(110)  # Or set widths individually if needed
  ) %>%
  sub_missing(columns = everything(), missing_text = "") %>%
  opt_row_striping() %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md("Note: Impact represent the unique contribution of each Web-CLIC dimension while controlling for the others.")
  )




