# what: connect to googleCloudStorage for storage in boards via pins

# packages ----------------------------------------------------------------

library(googleCloudStorageR)
library(gargle)
library(pins)


# set project -------------------------------------------------------------

# project id stored in project Renviron
project_id = Sys.getenv("GCS_PROJECT_ID")

# token for authentication with pins ()
# my_token = gargle::credentials_service_account(path = Sys.getenv("GCS_AUTH_FILE"))


# bucket ------------------------------------------------------------------

# create bucket
# not run
# gcs_create_bucket("bgg_bucket",
#                   project_id,
#                   versioning = T,
#                   location = "US")

# bucket info stored in project Renviron
my_bucket = Sys.getenv("GCS_DEFAULT_BUCKET")

# set upload limit
gcs_upload_set_limit(upload_limit = 200000000L)


# boards ------------------------------------------------------------------

# create board for storing models
model_board = 
        pins::board_gcs(
                bucket = my_bucket,
                prefix = "models/",
                versioned = T)

# create board for storing results
results_board = 
        pins::board_gcs(
                bucket = my_bucket,
                prefix = "results/",
                versioned = T)

