library(dotenv)

source("src/plots.R")
source("src/tex.R")
source("src/stats.R")
source("src/utils.R")

load_dot_env()

data <- get("cleaned_valid", envir = .GlobalEnv)
save_dir <- Sys.getenv("PLOTS_SAVE_DIR")
server_url <- Sys.getenv("PLOT_SERVER_URL")

upload_plot(get_group_dist_plot(cleaned_valid), save_dir, "group_dist.png", server_url)
upload_plot(get_gender_dist_plot(cleaned_valid), save_dir, "gender_dist.png", server_url)
upload_plot(get_age_dist_plot(cleaned_valid), save_dir, "age_dist.png", server_url)
upload_plot(get_education_level_dist_plot(cleaned_valid), save_dir, "education_dist.png", server_url)
upload_plot(get_instagram_usage_plot(cleaned_valid), save_dir, "instagram_usage_dist.png", server_url)
upload_plot(get_ers_box_plot(cleaned_valid), save_dir, "ers_box_plot.png", server_url)
upload_plot(get_clar_density_plot(cleaned_valid), save_dir, "clar_density_plot.png", server_url)
upload_plot(get_like_density_plot(cleaned_valid), save_dir, "like_density_plot.png", server_url)
upload_plot(get_info_density_plot(cleaned_valid), save_dir, "info_density_plot.png", server_url)
upload_plot(get_cred_density_plot(cleaned_valid), save_dir, "cred_density_plot.png", server_url)
upload_plot(get_clic_density_plot(cleaned_valid), save_dir, "clic_density_plot.png", server_url)
upload_plot(get_ranking_dist_plot(cleaned_valid), save_dir, "ranking_dist.png", server_url)

upload_tex(get_clic_repeated_measures_anova_tex(cleaned_valid), save_dir, "clic_anova.tex", server_url)
upload_tex(get_clic_impact_on_rank_tex(cleaned_valid), save_dir, "clic_impact_rank.tex", server_url)
