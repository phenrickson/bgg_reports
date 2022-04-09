# load 
source(here::here("scripts/load_packages.R"))
source(here::here("functions/theme_phil.R"))

library(bigrquery)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# authorize
bq_auth(email = "phil.henrickson@aebs.com")

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# query table of game info
active_games<-DBI::dbGetQuery(bigquerycon, 
                              'SELECT * FROM bgg.api_game_info') %>%
        select(-starts_with("rank"))

# table with types
game_types= DBI::dbGetQuery(bigquerycon, 
                            'SELECT * FROM bgg.api_game_categories')

# get previously loaded features to keep
load(here::here("local/types_selected.Rdata"))

# function for pivoting and making dummy variables
pivot_and_dummy_types = function(input_data, input_type) {
        
        # pivoting
        input_data %>%
                filter(type == input_type) %>%
                mutate(type_abbrev = substr(type, 1, 3)) %>%
                mutate(value = tolower(gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", value))))) %>%
                select(game_id, type, value) %>%
                mutate(type = paste(type, value, sep="_")) %>%
                mutate(has_type = 1) %>%
                select(-value) %>%
                pivot_wider(names_from = c("type"),
                            values_from = c("has_type"),
                            id_cols = c("game_id"),
                            names_sep = "_",
                            values_fn = min,
                            values_fill = 0)
        
}

# categories
game_categories = pivot_and_dummy_types(game_types,
                                        "category") %>%
        select(game_id,
               one_of(types_selected$selected_categories$tidied))

# family
game_families= pivot_and_dummy_types(game_types,
                                     "family") %>%
        select(game_id,
               one_of(types_selected$selected_families$tidied))

# mechanics
game_mechanics = pivot_and_dummy_types(game_types,
                                       "mechanic") %>%
        select(game_id,
               one_of(types_selected$selected_mechanics$tidied))

# designers
game_designers = pivot_and_dummy_types(game_types,
                                       "designer") %>%
        select(game_id,
               one_of(types_selected$selected_designers$tidied))

# publishers
game_publishers = pivot_and_dummy_types(game_types,
                                        "publisher") %>%
        select(game_id,
               one_of(types_selected$selected_publishers$tidied))

# artists
game_artists = pivot_and_dummy_types(game_types,
                                     "artist") %>%
        select(game_id,
               one_of(types_selected$selected_artists$tidied))

# games table
games_full= active_games %>%
        left_join(., game_mechanics,
                  by = c("game_id")) %>%
        left_join(., game_families,
                  by = c("game_id")) %>%
        left_join(., game_categories,
                  by = c("game_id")) %>%
        left_join(., game_designers,
                  by = c("game_id")) %>%
        left_join(., game_publishers,
                  by = c("game_id")) %>%
        left_join(., game_artists,
                  by = c("game_id")) %>%
        filter(yearpublished != 0)

# filter to most recent
games_model = games_full %>%
        filter(timestamp == max(timestamp))

        # mutate_at(c("yearpublished",
        #             "averageweight", 
        #             "playingtime",
        #             "minage"),
        #       .funs = ~ na_if(., 0)) %>%

# change names for types
game_types = game_types %>%
        mutate(value = tolower(gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", value)))))

# save two files to local
# one dataset ready for models
readr::write_rds(games_model,
                 file = here::here("experiments/data", "games_model.Rdata"))

# one dataset containing info on games
readr::write_rds(game_types,
                 file = here::here("experiments/data", "game_types.Rdata"))

rm(list = ls())
