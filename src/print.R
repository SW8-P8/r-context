source("src/plots.R")
source("src/stats.R")
source("src/tex.R")

data <- get("cleaned_valid", envir = .GlobalEnv)

# Demograhic plots
print(get_p_seq_dist_plot(data))
print(get_gender_dist_plot(data))
print(get_age_dist_plot(data))
print(get_education_level_dist_plot(data))
print(get_instagram_usage_plot(data))
print(get_ers_box_plot(data))
print(get_clar_density_plot(data))
print(get_like_density_plot(data))
print(get_info_density_plot(data))
print(get_cred_density_plot(data))
print(get_clic_density_plot(data))
print(get_combined_clic_density_plot(data))
print(get_ranking_dist_plot(data))
print(get_rank_coefficient_plot(data))

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
print(get_clic_repeated_measures_anova_tex(data))

# CLIC pairwise t test
print(get_clic_pairwise_prototype_t_test_results(data))
print(get_clic_pairwise_p_seq_t_test_results(data))
print(get_clic_pairwise_prototype_t_test_tex(data))
print(get_clic_pairwise_prototype_plot(data))

#######################################################

# RANK friedman test
print(get_rank_friedman_results(data))

# RANK anova
print(get_rank_anova_results(data))

# RANK wilcoxon
print(get_rank_wilcoxon_results(data))

# RANK placketluce
print(get_rank_placketluce_results(data))
print(get_rank_placketluce_coef_results(data))

#######################################################

# RANK-CLIC polr
print(get_rank_clic_polr_results(data))
print(get_rank_clic_polr_tex(data))

#######################################################

# INFO-SENS correlation
print(get_correlation_sens_info_plot(data))

# INFO-SENS spearman
print(get_spearman_sens_info_results(data))


