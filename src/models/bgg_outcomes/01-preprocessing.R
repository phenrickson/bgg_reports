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


# functions used in handling categorical features
source(here::here("src", "features", "categorical_features_functions.R"))

# function for estimating partial effects given categorical features
source(here::here("src", "features", "estimate_partial_effects_functions.R"))

# creates tidied games dataset for analysis
tidy_and_split_games = function(train_year,
                          min_ratings) {
        
        # apply consistent prep to full table
        prepped =
                analysis_games %>%
                # change zeros to missingness
                mutate_at(
                        vars(c("yearpublished",
                               "averageweight",
                               "average",
                               "bayesaverage",
                               "usersrated",
                               "stddev",
                               "minplayers",
                               "maxplayers",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime")),
                        ~ na_if(., 0))
        
        # training set
        # removes unreleased games
        # then removes games with data quality issues (drop, missinginess on yearpublished, etc)
        train = prepped %>%
                # filter to year
                filter(yearpublished <= train_year) %>%
                # drop games that weren't released or have data quality issues
                filter(!(game_id %in% c(
                        unreleased_games$game_id,
                        drop_games$game_id))
                ) %>%
                # filter out games with missingness on yearpublished
                filter(!is.na(yearpublished)) %>%
                # filter out games with missingness on average weight
              #  filter(!is.na(averageweight) & averageweight!=0) %>%
                # filter our kickstarter editions and big box editions
                #  filter(!(grepl("kickstarter|big box|mega box|megabox", tolower(name)))) %>%
                # filter to games with at least X votes
                filter(usersrated >= min_ratings) %>%
                # set yearpublished to numeric
                mutate(yearpublished = as.numeric(yearpublished)) %>%
                # get description
                left_join(., 
                          game_descriptions %>%
                                  select(game_id, description),
                          by = c("game_id")) %>%
                select(game_id,
                       name,
                       yearpublished,
                       image,
                       thumbnail,
                       averageweight,
                       average,
                       bayesaverage,
                       usersrated,
                       minage,
                       minplayers,
                       maxplayers,
                       playingtime,
                       minplaytime,
                       maxplaytime,
                       description,
                       load_ts)
        
        # validation set
        # games published after training year
        # still excludes those with key data quality issues (meet drop criteria; missingness on yearpublished)
        # but, keeps games with missigness on average weight
        # and applies no minimum ratings filter
        valid = 
                prepped %>%
                filter(yearpublished > train_year) %>%
                # drop games that weren't released or have data quality issues
                filter(!(game_id %in% c(
                        unreleased_games$game_id,
                        drop_games$game_id))
                ) %>%
                # filter out games with missingness on yearpublished
                filter(!is.na(yearpublished)) %>%
                # set yearpublished to numeric
                mutate(yearpublished = as.numeric(yearpublished)) %>%
                # get description
                left_join(., 
                          game_descriptions %>%
                                  select(game_id, description),
                          by = c("game_id")) %>%
                select(all_of(names(train)))
        
        # games not in train or valid
        other = 
                prepped %>%
                filter(!(game_id %in% c(train$game_id, valid$game_id))) %>%
                # get description
                left_join(., 
                          game_descriptions %>%
                                  select(game_id, description),
                          by = c("game_id")) %>%
                select(all_of(names(train)))
        
        
        return(list("train_games" = train,
                    "valid_games" = valid,
                    "other_games" = other))
        
        
}


# data splitting -----------------------------------------------------------


# get filtered games given training year
tidied_games = tidy_and_split_games(train_year = 2019,
                                    min_ratings = 25)
        
# get train
train_games = tidied_games$train_games

# get valid
valid_games = tidied_games$valid_games

# get all other
other_games = tidied_games$other_games


# create features for categorical variables -------------------------------------------

# use function
selected_categorical_variables = select_categorical_variables(train_games)

# get features (pivoted table of games with dummies for categories)
categorical_features = selected_categorical_variables$games_categorical_pivoted

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
rm(list=setdiff(ls(), c("selected_categorical_variables",
                        "categorical_features",
                        "train",
                        "valid",
                        "other")))

# modeling

