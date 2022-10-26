# connect to bigquery
require(odbc)
require(bigrquery)
require(DBI)
require(keyring)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# authenticate
bq_auth(email = 'phil.henrickson@aebs.com')

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

