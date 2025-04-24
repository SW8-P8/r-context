# Kør kun dette en gang
install.packages("devtools")
install.packages("dotenv")
library(devtools)
install_github("cloudyr/limer")

# Husk at lave en .env fil

#####################################

library(dotenv)
library(limer)

load_dot_env()

# Limer
# https://github.com/cloudyr/limer

options(lime_api = Sys.getenv("API_URL"))
options(lime_username = Sys.getenv("USERNAME"))
options(lime_password = Sys.getenv("PASSWORD"))

get_session_key()  # Log in

# Udkommenter for at se survey ids
# call_limer(method = "list_surveys")

# Alternativ måde at få responses på
# responses <- get_responses(395995)

responses <- base64_to_df(call_limer(method = "export_responses", 
                       params = list(iSurveyID = 395995, 
                                     sDocumentType = "csv", 
                                     sLanguageCode = "en", 
                                     sCompletionStatus = "complete", 
                                     sHeadingType = "code", 
                                     sResponseType = "short")))

