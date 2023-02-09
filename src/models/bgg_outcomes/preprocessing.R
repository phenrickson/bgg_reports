# what: pulls together datasets and applies preprocessing conditional on train/validation split

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
processed_board = board_folder(here::here("data", "processed"),
                     versioned = T)

# read in publisher allow list
publisher_allow_list = processed_board %>%
        pin_read("publisher_allow_list")


# functions ---------------------------------------------------------------

# functions used in creating features fore games
source(here::here("src", "features", "make_features_functions.R"))

# for plotting
source(here::here("src", "helpers", "theme_phil.R"))


# data splitting -----------------------------------------------------------

# use tidy and split games function to designate train test split

# get filtered games given training year

tidied_games = 
        # apply simple preprocessing
        tidy_games(analysis_games) %>%
        # apply custom split on training year, filtering to minimum ratings
        split_games(.,
                    end_train_year = end_train_year,
                    min_ratings = min_ratings)
        
# get train
train_games = tidied_games$train_games

# get valid
valid_games = tidied_games$valid_games

# get all other
other_games = tidied_games$other_games


# create features for categorical variables -------------------------------------------

# use categorical features function to select categorical features for 
selected_categorical_variables = create_categorical_variables(train_games)

# get features (pivoted table of games with dummies for categories)
categorical_features = selected_categorical_variables$games_categorical_pivoted

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
                  replace_na, 0)

# validation
valid = 
        valid_games %>%
        # join with categorical features
        left_join(.,
                  categorical_features,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(categorical_features))),
                  replace_na, 0)


# other
other = 
        other_games %>%
        # join with categorical features
        left_join(.,
                  categorical_features,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(categorical_features))),
                  replace_na, 0)


        
# categorical blueprint
rm(list=setdiff(ls(), c("categorical_mapping",
                        "train",
                        "valid",
                        "other")))

# modeling

