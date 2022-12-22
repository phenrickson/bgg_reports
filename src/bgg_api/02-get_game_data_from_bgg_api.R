# packages
require(tidyverse)
require(data.table)
require(foreach)
require(httr2)
require(xml2)
require(XML)
require(rvest)
require(purrr)
require(here)
require(polite)
require(magrittr)
require(assertthat)
require(furrr)
require(future)
require(butcher)

# functions for connecting to gcp
source(here("functions", "connect_to_gcp.R"))

# functions for submitting a game id to bgg api
# submit game ids to 'get_bgg_game_data' function
source(here("scripts","bgg_api", "bgg_api_functions.R"))

# get bgg ids
# get most recent time game id appeared
bgg_ids<-DBI::dbGetQuery(bigquerycon, 
                         'SELECT game_id, raw_name, tidy_name, MAX(upload_ts) as most_recent_by_game, MIN(upload_ts) as first_entry_by_game FROM bgg.scraped_bgg_ids
                              GROUP BY game_id, raw_name, tidy_name')

# get vector of unique ids
bgg_ids_vec = bgg_ids %>%
        select(game_id) %>%
        unique %>%
        pull

# get table
bgg_games_raw = get_bgg_games_data(bgg_ids_vec,
                              tidy = T,
                              toJSON = F)

# to json
save(bgg_games_raw,
        file = here("data", "api", paste("bgg_games_raw_", Sys.Date(), ".Rdata", sep="")))

rm(list=ls())
