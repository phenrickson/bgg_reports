# what: pull together datasets and applies preprocessing conditional on train/validation split

# dependencies:
# load analysis tables (locally) or run query

# packages ----------------------------------------------------------------

# load core packages needed for modeling
suppressPackageStartupMessages({
        
        # tidyverse/modeling
        library(tidyverse)
        library(tidymodels)
        library(tidyselect)
        
        # tidymodels preferences
        tidymodels_prefer()
        
        # recipes and workflows
        library(recipes)
        library(workflows)
        library(workflowsets)
        
        # additional
        library(magrittr)
        library(broom.mixed)
        library(data.table)
        library(tidytext)
        library(conflicted)
        library(lubridate)
        
        # ggplot
        library(ggforce)
        
        # pins
        library(pins)
        
}
)

# conflicts
suppressMessages({
        conflict_prefer("year", "lubridate")
        conflict_prefer("quarter", "lubridate")
        conflict_prefer("set_names", "magrittr")
        conflict_prefer("flatten", "purrr")
        conflict_prefer("tune", "tune")
        conflict_prefer("plotly", "layout")
})


# data --------------------------------------------------------------------


# load tables used in modeling
# local version
load(here::here("data", "local", "analysis_games_tables.Rdata"))

# # query from gcp (and update local)
# source(here::here("pull_analysis_games_tables.R"))

# publisher white list
processed_board = board_folder(here::here("data", "processed"), versioned = T)

# read in publisher allow list
publisher_allow_list = processed_board %>% pin_read("publisher_allow_list")



# functions ---------------------------------------------------------------


# functions used in creating features fore games
source(here::here("src", "features", "make_features_functions.R"))


# data splitting -----------------------------------------------------------

# use tidy and split games function to designate train test split
tidied_games = 
        # apply simple preprocessing
        tidy_games(analysis_games) %>%
        # apply custom split on training year, filtering to minimum ratings
        split_games(.,
                    end_train_year = end_train_year,
                    min_ratings = 0)
        
# get train
train_games = tidied_games$train_games


# set end valid year
end_valid_year = end_train_year +2

# get valid
valid_games = tidied_games$valid_games %>%
        filter(yearpublished <= end_valid_year)

# get test
test_games = tidied_games$valid_games %>%
        filter(yearpublished > end_valid_year)

# get all other
other_games = tidied_games$other_games


# filter training set with hurdle model -----------------------------------------------------


# read hurdle model
#model_board = board_folder(here::here("models", "board"))

# # load model
# hurdle_model = vetiver::vetiver_pin_read(board = model_board,
#                                    name = "hurdle_model")
# 
# # predict
# train_hurdle_preds =
#         # use hurdle model to predict probabiltiy that games in training set will hit minimum
#         # this essentially takes the form of downsampling the training set and removing a lot of the low quality entires
#         hurdle_model %>%
#         augment(hurdle_prep(train_games), type = 'prob')
# 
# 
# # keep games above a cutpoint, which in training was around 0.1
# train_hurdle =
#         train_hurdle_preds %>%
#         # above pre determined cutpoint
#         filter(.pred_yes >= .1) %>%
#         # keep only names in train games
#         select(names(train_games))
# 
# train_games = train_hurdle
# rm(train_hurdle)


# create features for categorical variables -------------------------------------------


# use categorical features function to select categorical features for 
selected_categorical_variables = 
        create_categorical_variables(train_games)

# get features (pivoted table of games with dummies for categories)
categorical_features = 
        selected_categorical_variables$games_categorical_pivoted

# keep categorical mapping
categorical_mapping = selected_categorical_variables$categorical_mapping

# add categorical features to training set
train = 
        train_games %>%
        # join with categorical features
        left_join(.,
                  categorical_features,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(categorical_features))),
                  replace_na, 0) %>%
        # apply preprocessing
        preprocess_categorical_games() %>%
        # log transform usersrated
        mutate(log_usersrated = log1p(usersrated))

# validation
valid = 
        valid_games %>%
        # join with categorical features
        left_join(.,
                  categorical_features,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(categorical_features))),
                  replace_na, 0) %>%
        # apply preprocessing
        preprocess_categorical_games() %>%
        # log transform usersrated
        mutate(log_usersrated = log1p(usersrated))

# test
test = 
        test_games %>%
        # join with categorical features
        left_join(.,
                  categorical_features,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(categorical_features))),
                  replace_na, 0) %>%
        # apply preprocessing
        preprocess_categorical_games() %>%
        # log transform usersrated
        mutate(log_usersrated = log1p(usersrated))


# other
other = 
        other_games %>%
        # join with categorical features
        left_join(.,
                  categorical_features,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(categorical_features))),
                  replace_na, 0) %>%
        # apply preprocessing
        preprocess_categorical_games() %>%
        # log transform usersrated
        mutate(log_usersrated = log1p(usersrated))

# categorical blueprint
rm(list=setdiff(ls(), c("categorical_mapping",
                        "end_train_year",
                        "train",
                        "valid",
                        "test",
                        "other")))

