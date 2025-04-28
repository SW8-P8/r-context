library(dotenv)
library(limer)
source("src/utils.R")

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
