source("src/plots.R")
source("src/stats.R")

data <- get("cleaned_valid", envir = .GlobalEnv)
print(get_clic_repeated_measures_anova_results(data))

# Demograhic plots
print(get_group_dist_plot(cleaned_valid))
print(get_gender_dist_plot(cleaned_valid))
print(get_age_dist_plot(cleaned_valid))
print(get_education_level_dist_plot(cleaned_valid))
print(get_instagram_usage_plot(cleaned_valid))
print(get_ers_box_plot(cleaned_valid))
print(get_clar_density_plot(cleaned_valid))
print(get_like_density_plot(cleaned_valid))
print(get_info_density_plot(cleaned_valid))
print(get_cred_density_plot(cleaned_valid))
print(get_clic_density_plot(cleaned_valid))
print(get_ranking_dist_plot(cleaned_valid))
print(get_clic_histogram_plot(cleaned_valid))
print(get_clic_qq_plot(cleaned_valid))
print(get_pairwise_rank_plot(cleaned_valid))
print(get_rank_coefficient_plot(cleaned_valid))

#######################################################

# CLIC normality tests
print(get_clic_shapiro_results(data))
print(get_clic_kolmogorov_results(data))
print(get_clic_anderson_darling_results(data))

# Visualize normality tests
print(get_clic_histogram_plot(data))
print(get_clic_qq_plot(data))

# Homogeneity of variance
print(get_clic_levenes_results(data))

# CLIC ANOVA
print(get_clic_repeated_measures_anova_results(data))

# CLIC pairwise t test
print(get_clic_pairwise_prototype_t_test_results(data))
print(get_clic_pairwise_group_t_test_results(data))

#######################################################

# RANK pairwise
print(get_pairwise_rank_plot(data))

# RANK friedman test
print(get_rank_friedman_results(data))

# RANK anova
print(get_rank_anova_results(data))

# RANK wilcoxon
print(get_rank_wilcoxon_results(data))

# RANK placketluce
print(get_rank_placketluce_results(data))
print(get_rank_placketluce_coef_results(data))
