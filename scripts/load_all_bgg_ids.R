# load the function
source(here::here("functions", "get_all_bgg_ids.R"))

# packages
library(tidyverse)
library(dplyr)
library(data.table)
library(foreach)
library(httr)
library(xml2)
library(rvest)

# run the function
# currently takes about an hour and 10 minutes
all_bgg_ids_raw = get_all_bgg_ids(
        minpages = 1,
        maxpages = 1366)

# check length

# save raw
readr::write_rds(all_bgg_ids_raw,
                 file = here::here("data", "all_bgg_ids_raw.Rdata"))

# tidy, trim down to only boardgames and cleanup
all_game_ids_tidied = all_bgg_ids_raw %>%
        filter(type == 'boardgame') %>%
        mutate_at(vars("V1", "V2", "V3"),
                  ~ as.numeric(gsub("\\)", "", gsub("\\(", "", .)))) %>%
        mutate(yearpublished = case_when(is.na(V1) ~ V2,
                                         is.na(V2) & is.na(V3) ~ V1,
                                         TRUE ~ V1)) %>%
        filter(!is.na(yearpublished)) %>%
        mutate(game_id = as.integer(game_id)) %>%
        select(date, url, page, type, game_id, name, yearpublished)

# save locally
readr::write_rds(all_game_ids_tidied,
                 file = here::here("data", "all_game_ids_tidied.Rdata"))

# now push this to GCP
# connect to sql
library(magrittr)
library(odbc)
library(bigrquery)
#library(bigQueryR)
library(DBI)
library(keyring)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

bq_auth(email = 'phil.henrickson@aebs.com')

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# bq object
bq_table<-as_bq_table(list(project_id = PROJECT_ID,
                              dataset_id = "bgg",
                              table_id = "api_all_game_ids"))
# confirm it is present
bq_table_exists(bq_table)

# push to this table via an append
dbWriteTable(bigquerycon,
             name = "api_all_game_ids",
             append = T,
             value = all_game_ids_tidied)

rm(list=ls())

