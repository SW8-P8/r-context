# Kør kun dette en gang
install.packages("devtools")
install.packages("dotenv")
library(devtools)
install_github("cloudyr/limer")

# Husk at lave en .env fil

#####################################

library(dotenv)
library(limer)

dotenv::load_dot_env()

options(lime_api = 'https://survey.bjeldbak.com/index.php/admin/remotecontrol')
options(lime_username = Sys.getenv("USERNAME"))
options(lime_password = Sys.getenv("PASSWORD"))

get_session_key()  # Log in

# Udkommenter for at se survey ids
# call_limer(method = "list_surveys")

responses <- get_responses(395995)
