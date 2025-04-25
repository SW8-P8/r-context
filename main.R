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
           sCompletionStatus = "complete", 
           sHeadingType = "code", 
           sResponseType = "short")))

filtered_responses <- filter_valid_responses(responses)

cleaned_responses <- clean_responses(filtered_responses)

print_group_dist_plot(cleaned_responses)
print_gender_dist_plot(cleaned_responses)
print_age_dist_plot(cleaned_responses)
print_education_level_dist_plot(cleaned_responses)
print_ranking_dist_plot(cleaned_responses)

release_session_key()

