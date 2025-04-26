library(dotenv)
library(limer)
source("utils.R")
source("plots.R")

# Limer:https://github.com/cloudyr/limer

load_dot_env()

options(lime_api = Sys.getenv("API_URL"))
options(lime_username = Sys.getenv("USERNAME"))
options(lime_password = Sys.getenv("PASSWORD"))

get_session_key()

responses <- base64_to_df(call_limer(method = "export_responses", params = list(
           iSurveyID = 395995, 
           sDocumentType = "csv", 
           sLanguageCode = "en", 
           sCompletionStatus = "all", 
           sHeadingType = "code", 
           sResponseType = "short")))

release_session_key()

filtered <- filter_responses(responses)

responses_valid <- filtered$valid
responses_almost_valid <- filtered$almost_valid
responses_semi_valid <- filtered$semi_valid
responses_invalid <- filtered$invalid

cleaned_valid <- clean_responses(responses_valid)
cleaned_almost_valid <- clean_responses(responses_almost_valid)
cleaned_semi_valid<- clean_responses(responses_semi_valid)
cleaned_invalid <- clean_responses(responses_invalid)

print_group_dist_plot(cleaned_valid)
print_gender_dist_plot(cleaned_valid)
print_age_dist_plot(cleaned_valid)
print_education_level_dist_plot(cleaned_valid)
print_instagram_usage_plot(cleaned_valid)
print_ers_box_plot(cleaned_valid)
print_clar_density_plot(cleaned_valid)
print_like_density_plot(cleaned_valid)
print_info_density_plot(cleaned_valid)
print_cred_density_plot(cleaned_valid)
print_clic_density_plot(cleaned_valid)
print_ranking_dist_plot(cleaned_valid)
