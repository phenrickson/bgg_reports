# refit on entire dataset and pin to deployed
library(vetiver)

# now refit on both train and valid
# combine train and valid
final = bind_rows(train,
                  valid) %>%
        # remove games without a geek rating
        filter(!is.na(bayesaverage)) %>%
        filter(usersrated >= 25) %>%
        filter(!is.na(averageweight))

# combine imputed train and valid, keeping only games with a geek rating
final_imputed = 
        impute_averageweight(
                bind_rows(train,
                          valid)
        ) %>%
        filter(!is.na(bayesaverage))

# fit averageweight
averageweight_final = 
        averageweight_model %>%
        fit(final)

# fit average
average_final = 
        average_model %>%
        fit(final_imputed)

# fit usersrated
usersrated_final = 
        usersrated_model %>%
        fit(final_imputed)

# fit bayesaverage
bayesaverage_final = 
        bayesaverage_model %>%
        fit(final_imputed)

# # 
# bayesaverage_final %>%
#         augment(test %>%
#                         mutate(yearpublished = 2015)) %>%
#         select(game_id, name, yearpublished, .pred) %>%
#         arrange(desc(.pred))

# vetiver -----------------------------------------------------------------


# averageweight
vetiver_averageweight = vetiver_model(model = averageweight_final,
                                        model_name = "averageweight_model",
                                        description = paste("regression model trained through",
                                                            max(valid$yearpublished), 
                                                            "to predict averageweight"))
# average
vetiver_average = vetiver_model(model = average_final,
                                      model_name = "average_model",
                                      description = paste("regression model trained through",
                                                          max(valid$yearpublished), 
                                                          "to predict average"))

# usersrated
vetiver_usersrated = vetiver_model(model = usersrated_final,
                                model_name = "usersrated_model",
                                description = paste("regression model trained through",
                                                    max(valid$yearpublished), 
                                                    "to predict (logged) usersrated"))

# bayesaverage
vetiver_bayesaverage = vetiver_model(model = bayesaverage_final,
                                   model_name = "bayesaverage_model",
                                   description = paste("regression model trained through",
                                                       max(valid$yearpublished), 
                                                       "to predict bayesaverage"))

# save to deploy board
deploy_board = model_board = board_folder(here::here("models", "deployed"), versioned = T)

# write
# averageweight
vetiver::vetiver_pin_write(deploy_board,
                           vetiver_averageweight)

# average
vetiver::vetiver_pin_write(deploy_board, 
                           vetiver_average)

# usersrated
vetiver::vetiver_pin_write(deploy_board, 
                           vetiver_usersrated)

# bayesaverage
vetiver::vetiver_pin_write(deploy_board, 
                           vetiver_bayesaverage)

# hurdle
vetiver::vetiver_pin_write(deploy_board, 
                           hurdle_model)
