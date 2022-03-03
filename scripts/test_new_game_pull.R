# get functions
source(here::here("scripts/load_packages.R"))
source(here::here("functions/get_bgg_data_from_github.R"))
source(here::here("functions/get_bgg_data_from_api.R"))
source(here::here("functions/convert_bgg_api_data_to_tables.R"))
source(here::here("functions/predict_bgg_outcomes.R"))
source(here::here("functions/average_col_func.R"))
source(here::here("functions/bayesaverage_col_func.R"))
source(here::here("functions/complexity_col_func.R"))


# load in modeling workflows
average_workflow = readr::read_rds(here::here("experiments/final_average_xgbTree_fit.Rds"))
complexity_workflow = readr::read_rds(here::here("experiments/final_complexity_xgbTree_fit.Rds"))
usersrated_workflow = readr::read_rds(here::here("experiments/final_usersrated_xgbTree_fit.Rds"))
template_average = readr::read_rds(here::here("experiments/final_template_average.Rds"))
template_usersrated= readr::read_rds(here::here("experiments/final_template_usersrated.Rds"))

# get games from today
bgg_today = get_bgg_data_from_github(Sys.Date())

set.seed(5)
ids = bgg_today %>%
        filter(game_release_year== 2022) %>%
        pull(game_id) 

# push through api
api_returned = get_bgg_api_data(ids)

# convert to tabular
api_tables = convert_bgg_api_data_to_tables(api_returned)

# convert to format for model
api_games_info = api_tables$games_info %>%
        mutate(numweights = as.character(numweights)) %>% # for some reason numweights is coming through as a character...?
        bind_rows(., template_average[0,]) %>% # bind to our template format
        select(one_of(names(template_average))) # keep only variables in that format
 
# predict with function
out = predict_bgg_outcomes(api_games_info)

# spread
spread_out = rbindlist(out) %>%
        spread(outcome, .pred) %>%
        mutate(usersrated = round(exp(usersrated),0),
               usersrated_est_complexity = round(exp(usersrated_est_complexity), 0)) %>%
        mutate(bayesaverage = 
                       ((5.5 * 1000) + (average * usersrated))/
                       (1000 + usersrated)) %>%
        mutate(`bayesaverage_est_complexity` = 
                       ((5.5*1000) + (average_est_complexity * usersrated_est_complexity))/
                                (1000+usersrated_est_complexity)) %>%
        mutate_if(is.numeric, round, 2) %>%
        select(.row, game_id, 
               average,
               bayesaverage,
               usersrated,
               complexity,
               average_est_complexity,
               bayesaverage_est_complexity,
               usersrated_est_complexity)

# join back up and take a look
preds = api_games_info %>% 
        select(yearpublished, game_id, name) %>%
        left_join(., spread_out,
                  by = "game_id") %>%
        select(yearpublished, game_id, name, everything()) %>%
        arrange(desc(average))

# display
preds %>%
        select(-.row) %>%
        arrange(desc(bayesaverage)) %>%
        mutate_at(c("yearpublished", "game_id"),
                  ~ as.character(.)) %>%
        select(-contains("est_complexity")) %>%
        mutate(usersrated = plyr::round_any(usersrated, 50)) %>%
        rename(Published = yearpublished,
               ID = game_id,
               Game = name,
               Average = average,
               GeekAverage = bayesaverage,
               UserRatings = usersrated,
               Complexity = complexity) %>%
        select(Published, ID, Game, UserRatings, Average, GeekAverage, Complexity) %>%
        arrange(desc(Average)) %>%
        flextable() %>%
        bg(j = "GeekAverage",
           bg = bayesaverage_col_func) %>%
        bg(j="Average",
          bg = average_col_func) %>%
        bg(j = "Complexity",
           bg = complexity_col_func) %>%       
        bold(j = c("UserRatings",
                   "Average",
                   "GeekAverage",
                   "Complexity"),
             part = "header") %>%
        flextable::align(j = c("UserRatings",
                               "Average",
                               "GeekAverage",
                               "Complexity"),
                         align = "center",
                         part = "all") %>%
        add_header_row(values = 
                               c("",
                                 "",
                                 "",
                                 "Estimated",
                                 "Estimated",
                                 "Estimated",
                                 "Estimated")) %>%
        merge_h(part = "header") %>%
        # hline(j = c("UsersRated",
        #                          "Average",
        #                          "GeekAverage",
        #                          "Complexity"), 
        #       part = "header") %>%
        autofit()


preds %>%
        select(-.row) %>%
        arrange(desc(bayesaverage)) %>%
        mutate_at(c("yearpublished", "game_id"),
                  ~ as.character(.)) %>%
        select(yearpublished, game_id, name, contains("est_complexity")) %>%
        rename(average = average_est_complexity,
               bayesaverage = bayesaverage_est_complexity,
               usersrated = usersrated_est_complexity) %>%
        arrange(desc(bayesaverage)) %>%
        flextable()



# # embed api call into function
# get_bgg_api_and_predict_average_and_complexity = function(input_ids) {
#         
#         # load functions for working with API
#         source(here::here("functions/get_bgg_data_from_api.R"))
#         source(here::here("functions/convert_bgg_api_data_to_tables.R"))
#         
#         # push through api
#         api_returned = get_bgg_api_data(input_ids)
#         
#         # convert to tabular
#         suppressMessages({
#                 api_tables = convert_bgg_api_data_to_tables(api_returned)
#         })
#         
#         # get models and template
#         average_workflow = readr::read_rds(here::here("experiments/final_average_xgbTree_fit.Rds"))
#         complexity_workflow = readr::read_rds(here::here("experiments/final_complexity_xgbTree_fit.Rds"))
#         template = readr::read_rds(here::here("experiments/final_template_average.Rds"))
#         
#         # convert to format for model
#         input_games_info = api_tables$games_info %>%
#                 mutate(numweights = as.character(numweights)) %>% # for some reason numweights is coming through as a character...?
#                 bind_rows(., template[0,]) %>% # bind to our template format
#                 select(one_of(names(template))) # keep only variables in that format
#         
#         # estimate bgg average 
#         estimated_average = average_workflow %>%
#                 predict(input_games_info) %>%
#                 mutate(outcome = "average") %>%
#                 mutate(.row = row_number()) %>%
#                 select(outcome, .pred, .row) %>%
#                 left_join(., input_games_info %>%
#                                   mutate(.row = row_number()) %>%
#                                   select(game_id, .row),
#                           by = c('.row'))
#         
#         # estimate complexity
#         estimated_complexity = complexity_workflow %>%
#                 predict(input_games_info) %>% 
#                 mutate(outcome = "complexity") %>%
#                 mutate(.row = row_number()) %>%
#                 select(outcome, .pred, .row) %>%
#                 left_join(., input_games_info %>%
#                                   mutate(.row = row_number()) %>%
#                                   select(game_id, .row),
#                           by = c('.row'))
#         
#         # now amend games info with estimate of complexity
#         input_games_estimated_complexity  = estimated_complexity %>%
#                 left_join(., input_games_info,
#                           by = c("game_id")) %>%
#                 select(-averageweight, -.row, -outcome) %>%
#                 rename(averageweight = .pred)
#         
#         # estimated bgg average given estimated complexity
#         estimated_average_complexity = average_workflow %>%
#                 predict(input_games_estimated_complexity) %>%
#                 mutate(.row = row_number()) %>%
#                 mutate(outcome = "average_estimated_complexity") %>%
#                 select(outcome, .pred, .row) %>%
#                 left_join(., input_games_info %>%
#                                   mutate(.row = row_number()) %>%
#                                   select(game_id, .row),
#                           by = c('.row'))
#         
#         # get output
#         out = list("api_tables" = api_tables,
#                    "api_games_info" = input_games_info,
#                    "estimated_average" = estimated_average,
#                    "estimated_complexity" = estimated_complexity,
#                    "estimated_average_complexity" = estimated_average_complexity)
#         
# }
# 
# # use function to predict api info
# foo = predict_bgg_outcomes(api_games_info)
# 
# # use function to send id to api and get prediction
# bar = get_bgg_api_and_predict_average_and_complexity(ids)
