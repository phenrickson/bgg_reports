# who: phil henrickson
# what: load scraped bgg ids to GCP
# when: 10/24/2022

# purpose of script
message("loading bgg ids to gcp...")

# packages
library(tidyverse)
library(dplyr)
library(data.table)
library(foreach)
library(here)
library(assertthat)

# gcp
library(odbc)
library(bigrquery)
library(DBI)
library(keyring)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# authenticate
bq_auth(email = 'phil.henrickson@aebs.com')

# establish connection to big query
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# mes

### get scraped file
# get files of scraped bgg ids
bgg_id_files = list.files(here("scraped"))

# get most recent ids file
most_recent_ids_file = 
        bgg_id_files %>%
        # conver to tibble
        as_tibble("value") %>%
        # separate file name by underscore
        separate(value, into = c("source", "data", "date"), sep="_") %>%
        # separate date and file type
        separate(date, into = c("date", "type"), sep = "\\.") %>%
        # get date
        mutate(date = as.Date(date)) %>%
        # filter to most recent date
        filter(date == max(date)) %>%
        # get first in case of a tie
        slice_head(n =1) %>%
        # combine back togther
        unite(path, c("source", "data", "date"), sep = "_") %>%
        unite(file, c("path", "type"), sep = ".") %>%
        pull(file)

# make sure this file exists
message(paste("assert that", most_recent_ids_file, "in folder"))
assert_that(most_recent_ids_file %in% bgg_id_files, msg = "bgg ids file not in folder")

# print the file
message(paste("loading", most_recent_ids_file))

# read in file
bgg_ids = fread(here("scraped", most_recent_ids_file)) %>%
        as_tibble %>%
        # arrange by page count
        arrange(page) %>%
        # convert to gcp schema and add timestamp
        transmute(page,
                  game_id,
                  raw_name,
                  tidy_name,
                  scraped_ts = timestamp,
                  upload_ts = Sys.time())

# print number of records in file
message(paste(nrow(bgg_ids), "records in", most_recent_ids_file))

### load to gcp
message(paste("loading", most_recent_ids_file, "to gcp"))

# find associated bq table
bq_table<-as_bq_table(list(project_id = PROJECT_ID,
                           dataset_id = "bgg",
                           table_id = "scraped_bgg_ids"))

# confirm bq table exists
message("assert that bq table exists")
assert_that(bq_table_exists(bq_table)==T,
            msg = "bq table does not exist")

# write to gcp
# load to table
dbWriteTable(bigquerycon,
             name = "scraped_bgg_ids",
             append = T,
             value = bgg_ids)

message("done.")
