# who: phil henrickson
# what: query gcp tables to get datasets of games for analysis
# when: 12/20/22

message("querying tables from gcp for analysis...")

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


# download tables ---------------------------------------------------------------

# get analysis games table
analysis_games <-bq_table_download(bq_project_query(PROJECT_ID,
                                                    'SELECT * FROM bgg.analysis_games')) %>%
        # change integers to numeric
        mutate_if(is.integer, as.numeric) %>%
        # change zeroes to NA
        mutate_at(c("averageweight",
                    "playingtime",
                    "minplaytime",
                    "maxplaytime",
                    "yearpublished"),
                  ~ case_when(. == 0 ~ NA_real_,
                              TRUE ~ .)) %>%
        arrange(desc(bayesaverage))

# get links
game_links<- bq_table_download(bq_project_query(PROJECT_ID,
                                                'SELECT * FROM bgg.api_game_links'))

# get player counts
game_playercounts = 
        bq_table_download(bq_project_query(PROJECT_ID,
                                           'SELECT * FROM bgg.api_game_playercounts'))

# get descriptions
game_descriptions = 
        bq_table_download(bq_project_query(PROJECT_ID,
                                           'SELECT * FROM bgg.api_game_descriptions'))

# create additional tables ------------------------------------------------

message("creating additional tables..")

# games that have not been released
unreleased_games = 
        game_links %>%
        filter(value == 'Admin: Unreleased Games') %>%
        transmute(
                type,
                value,
                id,
                game_id,
                load_ts
        )

# games to drop from from _all_ analysis
# games that are expansions for base games/fan expansions
# games flagged with admin (excluding unreleased games)
# games that are missing on yearpublished
drop_games = 
        # add in yearpublished and numweights for filtering
        game_links %>%
        left_join(.,
                  analysis_games %>%
                          select(game_id, name, yearpublished) %>%
                          distinct,
                  by = c("game_id")) %>%
        # remove upcoming releases from this set
        filter(value != 'Admin: Unreleased Games') %>%
        filter(
                (value %in% 
                       c('Expansion for Base-game',
                         'Fan Expansion',
                         '(Looking for a publisher)')
                )
                | grepl("Admin:", value) 
                | is.na(yearpublished) 
                | is.na(name) 
        ) %>%
        # big box kickstarter 
        select(game_id, name) %>%
        unique

# game compilations aka big boxes
game_compilations =
        game_links %>%
        filter(type == 'compilation') %>%
        transmute(
                type,
                name = value,
                id = id,
                game_id,
                load_ts)

# game reimplementations and editions
game_implementations =
        game_links %>%
        filter(type == 'implementation') %>%
        transmute(
                type,
                name = value,
                id = id,
                game_id,
                load_ts)

# games by bgg categories
game_categories =
        game_links %>%
        filter(type == 'category') %>%
        transmute(type,
                  value = value,
                  id = id,
                  game_id,
                  load_ts)

# games by bgg family categories
game_families =
        game_links %>%
        filter(type == 'family') %>%
        separate(value,
                 into = c("family_type", "family_value"),
                 sep= ": ",
                 extra = "merge",
                 fill = "right") %>%
        transmute(type,
                  family_type,
                  family_value,
                  value = paste(family_type, family_value),
                  id,
                  game_id,
                  load_ts)

# game designers
game_designers = 
        game_links %>%
        filter(type == 'designer') %>%
        transmute(type,
                  value,
                  id = id,
                  game_id,
                  load_ts)

# game publishers
game_publishers = 
        game_links %>%
        filter(type == 'publisher') %>%
        transmute(type,
                  value,
                  id = id,
                  game_id,
                  load_ts)

# game mechanics
game_mechanics = 
        game_links %>%
        filter(type == 'mechanic') %>%
        transmute(type,
                  value,
                  id = id,
                  game_id,
                  load_ts)

# game artists
game_artists = 
        game_links %>%
        filter(type == 'artist') %>%
        transmute(type,
                  value,
                  id = id,
                  game_id,
                  load_ts)

# save a copy of these tables for local use as an .Rdata object
message("saving a local copy...")
save(analysis_games,
     unreleased_games,
     drop_games,
     game_links,
     game_descriptions,
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

message("done.")




