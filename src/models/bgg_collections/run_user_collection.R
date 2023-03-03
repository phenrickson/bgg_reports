# what: run all functions for modeling user collection

# 0 - read in processed version of games 
source(here::here("src", "models", "bgg_collections", "setup.R"))

# get processed games
# # to run
# source(here::here("src", "models", "bgg_collections", "preprocess_games.R"))

# read from local layer
games_prepped = 
        pins::pin_read(processed_board,
          name = "games_prepped")

# set values needed
username = 'GOBBluth89'
end_train_year = 2020

# 1 - load user collection
source(here::here("src", "models", "bgg_collections", "load_user_collection.R"))

# 2 - join with preprocessed games
source(here::here("src", "models", "bgg_collections", "join_user_collection.R"))

# 3 - feature selection for user
source(here::here("src", "models", "bgg_collections", "selecting_user_features.R"))

# 4 - finalize user data
source(here::here("src", "models", "bgg_collections", "finalize_user_data.R"))

# 5 - model user collection
source(here::here("src", "models", "bgg_collections", "model_user_collection.R"))

