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
library(RcppSimdJson)

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

### get raw obj
# get files of scraped bgg ids
bgg_raw_folder = here("data", "api")
bgg_raw_files = list.files(bgg_raw_folder)

# get most recent ids file
most_recent_raw_file =
        bgg_raw_files %>%
        # conver to tibble
        as_tibble("value") %>%
        # contains "bgg_games_raw")
        filter(grepl("bgg_games_raw", value)) %>%
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
assert_that(most_recent_raw_file %in% bgg_raw_files, msg = "bgg raw file not in folder")

# print the file
message(paste("loading", most_recent_raw_file))

# read in file
load(here(bgg_raw_folder, most_recent_raw_file))

# # get data from json to tibble
# bgg_games_data = fparse(bgg_games_json$bgg_games_data)
bgg_games_data = bgg_games_raw$bgg_games_data

### get modeled tables to load to api

message("prepping api tables for GCP...")

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
                  thumbnail,
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
                           "childrensgames",
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

## analysis layer
# create a table containing most of the info i'll end up using for analysis here

# game playercounts
game_rec_playercounts = 
        game_playercounts %>% 
        mutate(value = tolower(gsub("\\s+", "", value))) %>%
        filter(numvotes > 0) %>%
        group_by(game_id) %>%
        mutate(total_votes = sum(numvotes)) %>%
        ungroup() %>%
        group_by(game_id, numplayers) %>%
        slice_max(numvotes, n=1, with_ties = F) %>% 
        group_by(game_id) %>%
        select(game_id, value, numplayers, total_votes, load_ts) %>% 
        pivot_wider(values_from = c("numplayers"),
                    names_from = c("value"), 
                    id_cols = c("game_id", "total_votes", "load_ts"),
                    names_prefix = c("playercount_"),
                    values_fn = ~ paste(.x, collapse=",")) %>%
        select(game_id, total_votes, playercount_best, playercount_recommended, playercount_notrecommended, load_ts) %>%
        ungroup() %>%
        transmute(game_id,
                  playercount_votes = total_votes,
                  playercount_best,
                  playercount_rec = playercount_recommended,
                  playercount_notrec = playercount_notrecommended,
                  load_ts)

# game ranks
game_rank_types = 
        game_ranks %>%
        select(game_id, rank_type, rank, load_ts) %>%
        pivot_wider(names_from = c("rank_type"),
                    values_from = c("rank"),
                    names_prefix = c("rank_"),
                    id_cols = c("game_id", "load_ts")) %>%
        transmute(game_id,
                  rank_boardgame,
                  rank_thematic,
                  rank_strategy = rank_strategygames,
                  rank_wargame = rank_wargames,
                  rank_family = rank_familygames,
                  rank_children = rank_childrensgames,
                  rank_cgs,
                  rank_abstract = rank_abstracts,
                  rank_party = rank_partygames,
                  load_ts)

# combine
analysis_games = 
        game_info %>%
        # get image
        left_join(.,
                  game_images %>%
                          select(game_id,
                                 image,
                                 thumbnail),
                  by = c("game_id")) %>%
        # get playercounts
        left_join(.,
                  game_rec_playercounts %>%
                          select(game_id,
                                 playercount_votes,
                                 playercount_best,
                                 playercount_rec,
                                 playercount_notrec),
                  by = c("game_id")) %>%
        # get ranks
        left_join(.,
                  game_rank_types %>%
                          select(game_id,
                                 rank_boardgame,
                                 rank_thematic,
                                 rank_strategy,
                                 rank_wargame,
                                 rank_family,
                                 rank_children,
                                 rank_cgs,
                                 rank_abstract,
                                 rank_party),
                  by = c("game_id")) %>%
        transmute(
                game_id,
                name,
                yearpublished,
                image, 
                thumbnail,
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
                playercount_votes,
                playercount_best,
                playercount_rec,
                playercount_notrec,
                rank_boardgame,
                rank_thematic,
                rank_strategy,
                rank_wargame,
                rank_family,
                rank_children,
                rank_cgs,
                rank_abstract,
                rank_party,
                load_ts)

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

### analysis
dbWriteTable(bigquerycon,
             name = "analysis_games",
             overwrite = T,
             value = analysis_games)

message("all tables loaded to GCP.")

gc()
rm(list = ls())
# tidying bgg xml using function
#tic("tidying bgg games xml")
#plan(multisession, workers = 4)
# bgg_games_data = tidy_bgg_data_xml(bgg_games_xml_obj)

#toc()