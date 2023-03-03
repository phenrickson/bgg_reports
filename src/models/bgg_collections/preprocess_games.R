# what: preprocess games data for use with modeling user collections


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



# prepare data for user analysis -----------------------------------------------------------------

message('prepping and imputing games...')

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
                
                # keep and nest prepped features
                out = out %>%
                        select(game_id, name, yearpublished, averageweight, .pred_hurdle) %>%
                        nest(prepped = c(averageweight, .pred_hurdle))
                
                return(out)
                
        }

# full set of games meeting criteria for inclusion
games_prepped = 
        prep_impute_and_hurdle(analysis_games,
                               average_model$prototype)

message('games preprocessing complete.')

# save to local layer
pins::pin_write(processed_board,
          games_prepped,
          description = 'data with imputed and predicted outcomes for user modeling')

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




 