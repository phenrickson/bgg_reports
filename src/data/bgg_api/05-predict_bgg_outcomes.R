# what: preprocess games data for use with modeling user collections

# what: setup for user modeling

message('preprocessing games for local layer')

message('loading packages and functions...')

# packages ----------------------------------------------------------------

# load core packages needed for modeling
suppressPackageStartupMessages({
        
        # tidyverse/modeling
        library(tidyverse)
        library(tidymodels)
        library(tidyselect)
        
        # set tidymodels preferences
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
        
        # vetiver
        library(vetiver)
        
        # tests
        library(testthat)
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

message('loading (local) data for games...')

# bgg data
# load tables used in modeling
# local version
load(here::here("data", "local", "analysis_games_tables.Rdata"))

# # query from gcp (and update local)
# source(here::here("pull_analysis_games_tables.R"))

# nested bgg data with all info
load(here::here("data", "local", "games_nested.Rdata"))

# publisher allow list
processed_board = board_folder(here::here("data", "processed"), versioned = T)

# read in publisher allow list
publisher_allow_list = processed_board %>% pin_read("publisher_allow_list")


# models ------------------------------------------------------------------


message('loading models for imputing bgg outcomes...')

# board of pre trained models
deployed_board = board_folder(here::here("models", "deployed"))


# load average model
average_model =
        vetiver::vetiver_pin_read(
                board = deployed_board,
                name = "average_model"
        )

# load usersrated model
usersrated_model =
        vetiver::vetiver_pin_read(
                board = deployed_board,
                name = "usersrated_model"
        )

# load bayesaverage model
bayesaverage_model =
        vetiver::vetiver_pin_read(
                board = deployed_board,
                name = "bayesaverage_model"
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


# impute data for modeling -----------------------------------------------------------------


message('prepping and imputing games for modeling...')

# function to prep games for user analysis by
# applying same tidying steps used in modeling bgg outcomes
# matching features of prototype for model
# imputing averageweight
# predicting with hurdle model

# , imputing, and then hurdling data
prep_impute_and_hurdle = 
        function(analysis_games,
                 prototype,
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
                                                 prototype = prototype) %>%
                        # impute missingness in averageweight
                        impute_averageweight()
                
                # now predict with hurdle model
                games_hurdle = 
                        hurdle_model %>%
                        # predict (via augment)
                        augment(
                                # prep for hurdle
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
                        # join with hurdle preds
                        left_join(.,
                                  games_hurdle,
                                  by = c("game_id", "name", "yearpublished")
                        )
                
                return(out)
                
        }

# full set of games meeting criteria for inclusion
games_prepped = 
        prep_impute_and_hurdle(analysis_games,
                               average_model$prototype)

message('imputation complete.')

# save to local layer
pin_write(processed_board,
          games_prepped,
          description = 'data with imputed and predicted outcomes for user modeling')


# predicting outcomes -----------------------------------------------------


# predict outcomes for games
predict_outcomes = function(data) {
        
        # averageweight
        predicted_averageweight = 
                averageweight_model %>%
                augment(data) %>%
                transmute(game_id, name, 
                          actual = averageweight,
                          .pred = .pred) %>%
                mutate(outcome = 'averageweight') %>%
                mutate(method = 'direct') %>%
                select(game_id, name, outcome, method, .pred, actual)
        
        # average
        predicted_average = 
                average_model %>%
                augment(data) %>%
                transmute(game_id, name, 
                          actual = average,
                          .pred = .pred) %>%
                mutate(outcome = 'average') %>%
                mutate(method = 'direct') %>%
                select(game_id, name, outcome, method, .pred, actual)
        
        # bayesaverage - direct model
        predicted_bayesaverage_d = 
                bayesaverage_model %>%
                augment(data) %>%
                transmute(game_id, name, 
                          actual = bayesaverage,
                          .pred = .pred) %>%
                mutate(outcome = 'bayesaverage') %>%
                mutate(method = 'direct') %>%
                select(game_id, name, outcome, method, .pred, actual)
                
        # usersrated
        predicted_usersrated = 
                usersrated_model %>%
                augment(data) %>%
                transmute(game_id, name, 
                          actual = usersrated,
                          .pred = plyr::round_any(exp(.pred), 50)) %>%
                mutate(outcome = 'usersrated') %>%
                mutate(method = 'direct') %>%
                select(game_id, name, outcome, method, .pred, actual)
                 
        
        # bayesaverage using direct model
        # predicted bayes average using other models
        predicted_bayesaverage_i = 
                bind_rows(predicted_average,
                          predicted_usersrated) %>%
                select(-actual) %>%
                pivot_wider(id_cols = c("game_id", "name", "method"),
                            names_from = c("outcome"),
                            values_from = c(".pred"),
                            names_prefix = c(".pred_")) %>%
                select(game_id, name, .pred_average, .pred_usersrated) %>%
                mutate(.pred = ((2000*5.5)+(.pred_usersrated*.pred_average)) /
                               (2000 + (.pred_usersrated))) %>%
                mutate(outcome = 'bayesaverage') %>%
                mutate(method = 'indirect') %>%
                # join back to get actual
                left_join(.,
                          data %>%
                                  select(game_id, bayesaverage) %>%
                                  rename(actual = bayesaverage),
                          by = c("game_id")) %>%
                select(game_id, name, outcome, method, .pred, actual)
        
        joined  = bind_rows(predicted_averageweight,
                            predicted_average,
                            predicted_usersrated,
                            predicted_bayesaverage_d,
                            predicted_bayesaverage_i)
        
        return(joined)
        
}
        
predicted_outcomes =
        predict_outcomes(games_prepped %>%
                                 filter(yearpublished > 2021)) %>%
        left_join(.,
                  games_prepped %>%
                          select(game_id,
                                 yearpublished)) %>%
        select(game_id, name, yearpublished, outcome, method, .pred, actual)


# write this out ----------------------------------------------------------



# remove objects
rm(prep_for_model_prototype,
   analysis_games,
   drop_games,
   game_artists,
   game_categories,
   game_compilations,
   game_descriptions,
   game_designers,
   game_families,
   game_images,
   game_implementations,
   game_mechanics,
   game_playercounts,
   game_publishers,
   unreleased_games,
   split_games,
   hurdle_model,
   average_model,
   averageweight_model,
   deployed_board)

# clear up memory
gc()




