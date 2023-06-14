# what: pull analysis tables from gcp analysis layer and save local copy

#  packages ---------------------------------------------------------------

suppressPackageStartupMessages(
        {
                require(tidyverse)
                require(tidymodels)
        }
)

# resolve conflict preferences
tidymodels_prefer()


# connect to gcp ----------------------------------------------------------

source(here::here("src", "helpers", "connect_to_gcp.R"))


# analysis layer ----------------------------------------------------------

# pull most recent load of game tables to analysis layer

# player counts and what not
game_playercounts = bq_table_download(bq_project_query(PROJECT_ID,
                                                      'SELECT * FROM bgg.api_game_playercounts
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.api_game_playercounts)'))

# analysis
analysis_games = bq_table_download(bq_project_query(PROJECT_ID,
                                                    'SELECT * FROM bgg.analysis_games
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_games)'))


# unreleased games
unreleased_games<- bq_table_download(bq_project_query(PROJECT_ID,
                                   'SELECT * FROM bgg.analysis_unreleased_games
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_unreleased_games)'))

# ganmes to drop due to data quality issues
drop_games<- bq_table_download(bq_project_query(PROJECT_ID,
                                                      'SELECT * FROM bgg.analysis_drop_games
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_drop_games)'))

# descriptions
game_descriptions<- bq_table_download(bq_project_query(PROJECT_ID,
                                                'SELECT * FROM bgg.analysis_game_descriptions
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_descriptions)'))

# images
game_images<- bq_table_download(bq_project_query(PROJECT_ID,
                                                       'SELECT * FROM bgg.analysis_game_images
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_images)'))

# categories
game_categories<- bq_table_download(bq_project_query(PROJECT_ID,
                                                 'SELECT * FROM bgg.analysis_game_categories
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_categories)'))

# compilations
game_compilations<- bq_table_download(bq_project_query(PROJECT_ID,
                                                     'SELECT * FROM bgg.analysis_game_compilations
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_compilations)'))

# designers
game_designers<- bq_table_download(bq_project_query(PROJECT_ID,
                                                       'SELECT * FROM bgg.analysis_game_designers
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_designers)'))

# publishers
game_publishers<- bq_table_download(bq_project_query(PROJECT_ID,
                                                    'SELECT * FROM bgg.analysis_game_publishers
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_publishers)'))

# families
game_families<- bq_table_download(bq_project_query(PROJECT_ID,
                                                     'SELECT * FROM bgg.analysis_game_families
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_families)'))

# implementations
game_implementations<- bq_table_download(bq_project_query(PROJECT_ID,
                                                   'SELECT * FROM bgg.analysis_game_implementations
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_implementations)'))

# artists
game_artists<- bq_table_download(bq_project_query(PROJECT_ID,
                                                          'SELECT * FROM bgg.analysis_game_artists
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_artists)'))

# mechanics
game_mechanics<- bq_table_download(bq_project_query(PROJECT_ID,
                                                  'SELECT * FROM bgg.analysis_game_mechanics
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_mechanics)'))


# save a copy of these tables for local use as an .Rdata object
message("saving a local copy...")
save(analysis_games,
     unreleased_games,
     drop_games,
     game_descriptions,
     game_images,
     game_categories,
     game_compilations,
     game_designers,
     game_families,
     game_implementations,
     game_playercounts,
     game_publishers,
     game_artists,
     game_mechanics,
     file = here::here("data", "local", "analysis_games_tables.Rdata"))


