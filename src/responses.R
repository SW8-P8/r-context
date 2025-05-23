library(dotenv)
library(limer)
source("src/cleaner.R")

# Limer:https://github.com/cloudyr/limer
# load_dot_env()
# options(lime_api = Sys.getenv("API_URL"))
# options(lime_username = Sys.getenv("USERNAME"))
# options(lime_password = Sys.getenv("PASSWORD"))
# get_session_key()
# 
# responses <- base64_to_df(call_limer(method = "export_responses", params = list(
#   iSurveyID = 395995, 
#   sDocumentType = "csv", 
#   sLanguageCode = "en", 
#   sCompletionStatus = "all", 
#   sHeadingType = "code", 
#   sResponseType = "short")))
# 
# release_session_key()

responses <- read.csv("responses.csv", header = TRUE)

cleaned_valid <- clean_responses(responses)
