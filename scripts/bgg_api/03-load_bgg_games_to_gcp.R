# who: phil henrickson
# what: tidy and load bgg games xml to GCP
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
library(tictoc)
library(future)
library(jsonlite)

# gcp
library(odbc)
library(bigrquery)
library(DBI)
library(keyring)

# connect to gcp
source(here("functions", "connect_to_gcp.R"))

# functions used
source(here("scripts","bgg_api", "bgg_api_functions.R"))

# pull from gcp
bgg_ids<-DBI::dbGetQuery(bigquerycon, 
                         'SELECT game_id, raw_name, tidy_name, MAX(upload_ts) as most_recent_by_game FROM bgg.scraped_bgg_ids
                              GROUP BY game_id, raw_name, tidy_name')

# get vector of unique ids
bgg_ids_vec = bgg_ids %>%
        select(game_id) %>%
        unique %>%
        pull

### get json obj
# get files of scraped bgg ids
bgg_json_folder = here("data", "api")
bgg_json_files = list.files(bgg_json_folder)

# get most recent ids file
most_recent_json_file = 
        bgg_json_files %>%
        # conver to tibble
        as_tibble("value") %>%
        # contains "bgg_games_json")
        filter(grepl("bgg_games_json", value)) %>%
        # separate file name by underscore
        separate(value, into = c("source", "table", "obj", "date"), sep="_") %>%
        # separate date and file type
        separate(date, into = c("date", "type"), sep = "\\.") %>%
        # get date
        mutate(date = as.Date(date)) %>%
        # filter to most recent date
        filter(date == max(date)) %>%
        # get first in case of a tie
        slice_head(n =1) %>%
        # combine back togther
        unite(path, c("source","table", "obj", "date"), sep = "_") %>%
        unite(file, c("path", "type"), sep = ".") %>%
        pull(file)

# make sure this file exists
assert_that(most_recent_json_file %in% bgg_json_files, msg = "bgg json file not in folder")

# print the file
message(paste("loading", most_recent_json_file))

# read in file
load(here(bgg_json_folder, most_recent_json_file))

# get data from json to tibble
bgg_games_data = fromJSON(bgg_games_json$bgg_games_data) %>%
        as_tibble

### get modeled tables to match gcp tables

message("prepping tables for GCP...")

# links
game_links = 
        bgg_games_data %>%
        filter(type == 'boardgame') %>%
        mutate(row = row_number()) %>%
        select(game_id, links) %>%
        mutate(is_df = map_lgl(links, ~ is.data.frame(.x))) %>%
        filter(is_df == T) %>%
        select(game_id, links) %>%
        unnest(links) %>%
        transmute(game_id,
                  type = gsub("boardgame", "", type),
                  id,
                  value,
                  load_ts = Sys.time())

# game names
game_names = 
        bgg_games_data %>%
        filter(type == 'boardgame') %>%
        select(game_id, names) %>%
        unnest(names) %>%
        type_convert() %>%
        transmute(game_id,
                  type,
                  value,
                  sortindex,
                  load_ts = Sys.time())

# game ids
game_ids = bgg_games_data %>%
        filter(type == 'boardgame') %>%
        select(game_id) %>%
        left_join(., 
                  game_names %>%
                          filter(type == 'primary') %>%
                          transmute(game_id,
                                    name = value),
                  by = 'game_id') %>%
        # get bgg's raw name for each game
        left_join(.,
                  bgg_ids %>%
                          select(game_id, raw_name, most_recent_by_game) %>%
                          group_by(game_id) %>%
                          slice_max(order_by = most_recent_by_game,
                                    n=1,
                                    with_ties = F) %>%
                          ungroup(),
                  by = c("game_id")) %>%
        # get yearpublished
        transmute(game_id,
                  name,
                  raw_name,
                  load_ts = Sys.time())
        
# expansion ids
game_expansions = game_links %>%
        filter(type == 'expansion') %>%
        transmute(game_id, 
                  expansion_id = id,
                  expansion_name = value,
                  load_ts = Sys.time())


# descriptions
game_descriptions = 
        bgg_games_data %>%
        filter(type == 'boardgame') %>%
        select(game_id, info) %>%
        unnest(info) %>%
        type_convert() %>%
        transmute(game_id,
                  description,
                  load_ts = Sys.time())

# images
game_images = 
        bgg_games_data %>%
        filter(type == 'boardgame') %>%
        select(game_id, info) %>%
        unnest(info) %>%
        type_convert() %>%
        # remove missingness
        filter(!is.na(image)) %>%
        transmute(game_id,
                  image,
                  load_ts = Sys.time())

# player counts
game_playercounts = 
        bgg_games_data %>%
        filter(type == 'boardgame') %>%
        select(game_id, polls) %>%
        unnest(polls) %>%
        type_convert() %>%
        transmute(game_id,
                  value,
                  numvotes,
                  numplayers,
                  load_ts = Sys.time())
                 
# game ranks
game_ranks = 
        bgg_games_data %>%
        filter(type == 'boardgame') %>%
        select(game_id, ranks) %>%
        unnest(ranks) %>%
        filter(name %in% c("abstracts",
                           "boardgame", 
                           "childresngames",
                           "cgs",
                           "familygames",
                           "partygames",
                           "strategygames",
                           "thematic",
                           "wargames")) %>%
        mutate(bayesaverage = case_when(bayesaverage == 'Not Ranked' ~ NA_character_,
                                        TRUE ~ bayesaverage),
               value = case_when(value == 'Not Ranked' ~ NA_character_,
                                 TRUE ~ value)) %>%
        type_convert() %>%
        transmute(game_id,
                  rank_type = name,
                  rank = value,
                  bayesaverage,
                  load_ts = Sys.time())

# info
game_info = 
        bgg_games_data %>%
        filter(type == 'boardgame') %>%
        select(game_id, info, statistics) %>%
        unnest(c(info, statistics)) %>%
        # add names
        left_join(.,
                  game_names %>%
                          filter(type == 'primary') %>%
                          transmute(game_id,
                                    name = value),
                  by = c("game_id")) %>%
        transmute(game_id,
                  name,
                  yearpublished,
                  averageweight,
                  average,
                  bayesaverage,
                  usersrated,
                  stddev,
                  minage,
                  minplayers,
                  maxplayers,
                  playingtime,
                  minplaytime,
                  maxplaytime,
                  numcomments,
                  numweights,
                  owned,
                  trading,
                  wanting,
                  wishing,
                  load_ts = Sys.time()
        )

### now load to GCP

message("loading games to GCP...")

# game info
dbWriteTable(bigquerycon,
             name = "api_game_info",
             append = T,
             value = game_info)

# game ids
dbWriteTable(bigquerycon,
             name = "api_game_ids",
             overwrite = T,
             value = game_ids)

# game expansions
dbWriteTable(bigquerycon,
             name = "api_game_expansions",
             overwrite = T,
             value = game_expansions)

# game expansions
dbWriteTable(bigquerycon,
             name = "api_game_names",
             overwrite = T,
             value = game_names)

# game links
dbWriteTable(bigquerycon,
             name = "api_game_links",
             overwrite = T,
             value = game_links)

# game ranks
dbWriteTable(bigquerycon,
             name = "api_game_ranks",
             overwrite = T,
             value = game_ranks)

# game playercounts
dbWriteTable(bigquerycon,
             name = "api_game_playercounts",
             overwrite = T,
             value = game_playercounts)

# game images
dbWriteTable(bigquerycon,
             name = "api_game_images",
             overwrite = T,
             value = game_images)

# game descriptions
dbWriteTable(bigquerycon,
             name = "api_game_descriptions",
             overwrite = T,
             value = game_descriptions)

message("all tables loaded to GCP.")
# tidying bgg xml using function
#tic("tidying bgg games xml")
#plan(multisession, workers = 4)
# bgg_games_data = tidy_bgg_data_xml(bgg_games_xml_obj)

#toc()