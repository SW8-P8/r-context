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


filtered <- filter_responses(responses)

valid_responses <- filtered$valid
semi_valid_responses <- filtered$semi_valid
invalid_responses <- filtered$invalid

cleaned_valid <- clean_responses(valid_responses)
cleaned_semi_valid<- clean_responses(semi_valid_responses)
cleaned_invalid <- clean_responses(invalid_responses)

print_group_dist_plot(cleaned_valid)
print_gender_dist_plot(cleaned_valid)
print_age_dist_plot(cleaned_valid)
print_education_level_dist_plot(cleaned_valid)
print_ranking_dist_plot(cleaned_valid)

release_session_key()

