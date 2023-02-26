# what: train models for user(s) bgg collection


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
        library(ggthemes)
        library(ggforce)
        library(ggfortify)
        
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



# functions ---------------------------------------------------------------

# bgg collection 
source(here::here("src", "data", "bgg_collections", "bgg_collection_functions.R"))

# bgg outcomes
source(here::here("src", "data", "bgg_outcomes", "bgg_outcomes_functions.R"))

# data --------------------------------------------------------------------


# bgg data
# load tables used in modeling
# local version
load(here::here("data", "local", "analysis_games_tables.Rdata"))

# # query from gcp (and update local)
# source(here::here("pull_analysis_games_tables.R"))



# models ------------------------------------------------------------------


# board of pre trained models
deployed_board = board_folder(here::here("models", "deployed"))
        

# load average model
average_model =
        vetiver::vetiver_pin_read(
                board = deployed_board,
                name = "average_model"
        )

# load averageweight model
averageweight_model =
        vetiver::vetiver_pin_read(
                board = deployed_board,
                name = "averageweight_model"
        )

# load hurdle model
hurdle_model =
        vetiver::vetiver_pin_read(
                board = deployed_board,
                name = "hurdle_model"
        )

# prep data
tidied_games = 
        tidy_games(analysis_games) %>%
        split_games(.,
                    end_train_year = 2021,
                    min_ratings = 0)



# prepare -----------------------------------------------------------------


# function to prep games for user analysis by first prepping, imputing, and then hurdling data
prep_impute_and_hurdle = 
        function(analysis_games,
                 end_train_year = 2021,
                 min_ratings = 30){
                
                # prep data and split
                tidied_games = 
                        tidy_games(analysis_games) %>%
                        split_games(.,
                                    end_train_year = end_train_year,
                                    min_ratings = min_ratings)
                
                # impute averageweight
                games_imputed = 
                        # combine
                        bind_rows(tidied_games$train_games,
                                  tidied_games$valid_games,
                                  tidied_games$other_games) %>%
                        # prep for model prototype using average
                        # also removes games that are missing yearpublished
                        prep_for_model_prototype(.,
                                                 prototype = average_model$prototype) %>%
                        # impute missingness in averageweight
                        impute_averageweight()
                
                # now predict with hurdle model
                games_hurdle = 
                        hurdle_model %>%
                        augment(
                                hurdle_prep(
                                        games_imputed
                                )
                        ) %>%
                        transmute(game_id, 
                                  name, 
                                  yearpublished,
                                  .pred_hurdle = .pred_yes)
                
                # join back up
                out = 
                        games_imputed %>%
                        left_join(.,
                                  games_hurdle,
                                  by = c("game_id", "name", "yearpublished")
                        )
                
                return(out)
                
        }

# full set of games meeting criteria for inclusion
games_prepped = 
        prep_impute_and_hurdle(analysis_games)



 