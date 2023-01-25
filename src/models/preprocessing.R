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
source(here::here("src", "functions", "preprocessing_functions.R"))

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

# estimates partial effects given categorical features
source(here::here("src", "functions", "estimate_partial_effects.R"))


# data splitting -----------------------------------------------------------


# get filtered games given training year
tidied_games = tidy_and_split_games(train_year = 2019,
                                    min_ratings =50)
        
# get train
train_games = tidied_games$train_games

# get valid
valid_games = tidied_games$valid_games

# get all other
other_games = tidied_games$other_games

# process categorical variables -------------------------------------------

# select categorical variables meeting minimum n selection

# mechanics
selected_mechanics =
        game_mechanics %>%
        # filter to games in train
        filter(game_id %in% train_games$game_id) %>%
        # min games filter
        group_by(id) %>%
        mutate(n_games = n_distinct(game_id)) %>%
        ungroup() %>%
        filter(n_games > 10) %>%
        distinct(type, id, value)

# categories
selected_categories = 
        game_categories %>%
        # filter games in training
        filter(game_id %in% train_games$game_id) %>%
        # min games filter
        group_by(id) %>%
        mutate(n_games = n_distinct(game_id)) %>%
        ungroup() %>%
        filter(n_games > 10) %>%
        distinct(type, id, value)

# families
selected_families = 
        game_families %>%
        # filter games in training
        filter(game_id %in% train_games$game_id) %>%
        # remove issues of leakage
        filter(!grepl("Admin Better Description", value)) %>%
        filter(!grepl("Digital Implementations", value)) %>%
        filter(!grepl("Misc", value)) %>%
        filter(!grepl("Unreleased", value)) %>%
        filter(!grepl("Upcoming Releases", value)) %>%
        filter(!grepl("Components Game Trayzinside", value)) %>%
        # min games filter
        group_by(id) %>%
        mutate(n_games = n_distinct(game_id)) %>%
        ungroup() %>%
        filter(n_games > 100) %>%
        distinct(type, id, value)


# estimates for designers and artists to reduce the cardinality

message("fitting a lasso to select game designers...")
designer_list = 
        estimate_partial_effects(train_games,
                                 game_designers,
                                 outcome = 'bayesaverage',
                                 min_games = 5) %$%
        partial_effects %>%
        pull(id)

message("fitting a lasso to select game artists...")
artist_list =
        estimate_partial_effects(train_games,
                                 game_artists,
                                 outcome = 'bayesaverage',
                                 min_games = 5) %$%
        partial_effects %>%
        pull(id)

# designers
selected_designers = 
        game_designers %>%
        # filter to designers in list
        filter(id %in% designer_list) %>%
        distinct(type, id, value)

# artists
selected_artists = 
        game_artists %>%
        # filter to artists in list
        filter(id %in% artist_list) %>%
        distinct(type, id, value)

# publishers
selected_publishers = 
        game_publishers %>%
        # allow list
        filter(id %in% publisher_allow_list) %>%
        distinct(type, id, value)


# bind selections together
categorical_selected = 
        bind_rows(selected_designers,
                  selected_categories,
                  selected_mechanics,
                  selected_publishers,
                  selected_families,
                  selected_artists) %>%
        select(type, id, value) %>%
        mutate(include = 1)

# pivot games
games_categorical_pivoted = 
        # bind games and categorical togther
        bind_rows(game_designers,
                  game_categories,
                  game_mechanics,
                  game_publishers,
                  game_families,
                  game_artists) %>%
        select(game_id, type, id, value) %>%
        # join up with those selected; keep only ones we've marked to include
        left_join(.,
                  categorical_selected,
                          by = c("type", "id", "value")) %>%
        # keep only those we selected
        filter(include == 1) %>%
        select(game_id, type, id, value) %>%
        # now pivot
        pivot_categorical_variables() 

# make mapping
categorical_mapping = 
        categorical_selected %>%
        select(type, id, value) %>%
        distinct %>%
        mutate(tidied = abbreviate_categorical(value)) %>%
        select(type, id, value, tidied)

# combine with training set
train = 
        train_games %>%
        left_join(.,
                  games_categorical_pivoted,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(games_categorical_pivoted))),
                  replace_na, 0)

# validation
valid = 
        valid_games %>%
        left_join(.,
                  games_categorical_pivoted,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(games_categorical_pivoted))),
                  replace_na, 0)

# other
other = 
        other_games %>%
        left_join(.,
                  games_categorical_pivoted,
                  by = c("game_id")) %>%
        # replace NAs in any of the categorical dummies with 0s
        mutate_at(vars(any_of(names(games_categorical_pivoted))),
                  replace_na, 0)

        
# categorical blueprint
rm(list=setdiff(ls(), c("categorical_mapping",
                        "categorical_selected",
                        "train",
                        "valid",
                        "other")))

# modeling

