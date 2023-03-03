# pull analysis tables from gcp analysis layer


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

# save nested version of games locally
games_nested = 
        analysis_games %>%
        # change zeros to missingness
        mutate_at(
                vars(c("yearpublished",
                       "averageweight",
                       "average",
                       "bayesaverage",
                       "stddev",
                       "minplayers",
                       "maxplayers",
                       "minage",
                       "playingtime",
                       "minplaytime",
                       "maxplaytime")),
                ~ na_if(., 0)) %>%
        # change some integers to numeric
        mutate_at(
                vars(c("yearpublished",
                       "minage",
                       "minplayers",
                       "maxplayers",
                       "usersrated",
                       "playingtime",
                       "minplaytime",
                       "maxplaytime")),
                as.numeric
        ) %>%
        # arrange by game id
        arrange(game_id) %>%
        # bgg_info 
        nest(bgg_info = c(yearpublished, minage, minplayers, maxplayers, playingtime, minplaytime, maxplaytime)) %>%
        # nest bgg outcomes
        nest(bgg_outcomes = c(averageweight, average, bayesaverage, usersrated, stddev)) %>%
        # nest bgg community 
        nest(bgg_community = c(numcomments, numweights, owned, trading, wanting, wishing)) %>%
        # nest images
        nest(images = c(image, thumbnail)) %>%
        # nest ranks
        nest(ranks = starts_with("rank_")) %>%
        # nest playercounts
        nest(playercounts = starts_with("playercount")) %>%
        # join with nested data
        # description
        left_join(.,
                  game_descriptions %>%
                          select(-load_ts) %>%
                          nest(description = description),
                  by = c("game_id")) %>%
        # categories
        left_join(.,
                  game_categories %>% 
                          select(-load_ts) %>%
                          nest(categories = -game_id),
                  by = c("game_id")
        ) %>%
        # mechanics
        left_join(.,
                  game_mechanics %>% 
                          select(-load_ts) %>%
                          nest(mechanics = -game_id),
                  by = c("game_id")
        ) %>%
        # designers
        left_join(.,
                  game_designers %>% 
                          select(-load_ts) %>%
                          nest(designers = -game_id),
                  by = c("game_id")
        ) %>%
        # artists
        left_join(.,
                  game_artists %>% 
                          select(-load_ts) %>%
                          nest(artists = -game_id),
                  by = c("game_id")
        ) %>%
        # families
        left_join(.,
                  game_families %>% 
                          select(-load_ts) %>%
                          nest(families = -game_id),
                  by = c("game_id")
        ) %>%
        # publishers
        left_join(.,
                  game_publishers %>% 
                          select(-load_ts) %>%
                          nest(publishers = -game_id),
                  by = c("game_id")
        ) %>%
        # implementations
        left_join(.,
                  game_implementations %>%
                          select(-load_ts) %>%
                          nest(implementations = -game_id),
                  by = c("game_id")
        ) %>%
        # compilations
        left_join(.,
                  game_compilations %>%
                          select(-load_ts) %>%
                          nest(compilations = -game_id),
                  by = c("game_id")
        )
        

message("saving games_nested table locally")
save(games_nested,
     file = here::here("data", "local", "games_nested.Rdata"))
