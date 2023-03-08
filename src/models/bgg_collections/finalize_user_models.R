# fits

message("finalizing model fits...")

# function to last fit workflows on training set
make_wflow_last_fit = function(workflow_set_rs,
                          id) {
        
        message("last fit for ", id)
        
        workflow_set_rs %>%
                extract_workflow(id = id) %>%
                finalize_workflow(parameters = best_tunes %>%
                                          filter(wflow_id == id)) %>%
                last_fit(split = valid_split,
                         metrics = tune_metrics)
        
}

# get last fits for each mod
last_fits = 
        map(workflow_set_rs$wflow_id,
            ~ make_wflow_last_fit(workflow_set_rs,
                                  .x) %>%
                    mutate(wflow_id = .x) %>%
                    select(wflow_id, everything())) %>%
        bind_rows()

# metrics on validation set
last_fits %>%
        select(wflow_id, 
               .metrics) %>%
        unnest(.metrics) %>%
        arrange(.metric) %>%
        select(wflow_id, .metric, .estimator, .estimate)

# look at predictions
last_fits %>%
        select(wflow_id,
               .predictions) %>%
        unnest(.predictions) %>%
        left_join(.,
                  valid_games %>%
                          select(game_id, name, yearpublished) %>%
                          mutate(.row = row_number() + nrow(train_games))) %>%
        select(wflow_id, .pred_yes, ever_owned, game_id, name, yearpublished) %>%
        select(wflow_id, game_id, name, ever_owned, .pred_yes, yearpublished) %>%
        pivot_wider(id_cols = c("yearpublished", "game_id", "name", "ever_owned"),
                    values_from = c(".pred_yes"),
                    names_from = c("wflow_id"))  %>%
        mutate_if(is.numeric, round, 3) %>%
        view()

# # pred test
# last_fits %>%
#         mutate(preds = map(.workflow,
#                            ~ .x %>%
#                                    augment(test_games %>%
#                                                    filter(yearpublished == 2025)) %>%
#                                #    filter(.pred_hurdle > .5) %>%
#                                    select(.pred_yes, ever_owned, game_id, name, yearpublished) %>%
#                                    mutate_if(is.numeric, round, 3) %>%
#                                    arrange(desc(.pred_yes)))) %>%
#         select(wflow_id, preds) %>%
#         unnest(preds) %>%
#         select(wflow_id, game_id, name, ever_owned, .pred_yes, yearpublished) %>%
#         pivot_wider(id_cols = c("yearpublished", "game_id", "name", "ever_owned"),
#                     values_from = c(".pred_yes"),
#                     names_from = c("wflow_id"))  %>%
#         mutate_if(is.numeric, round, 3) %>%
#         view()
# 
#          
# 
# # # # coefs
# # last_fits %>%
# #         head(1) %>%
# #         pluck(".workflow", 1) %>%
# #         extract_fit_parsnip() %>%
# #         tidy() %>%
# #         arrange(desc(estimate)) %>%
# #         filter(term!='(Intercept)') %>%
# #         filter(estimate !=0 ) %>%
# #         slice_max(order_by = abs(estimate), n =80) %>%
# #         ggplot(aes(x=estimate,
# #                    y=reorder(term, estimate)))+
# #         geom_point()
# # 
# # 
# # # # cart
# # # glmnet_last_fit = workflow_set_rs %>%
# # #         extract_workflow(id = 'preproc_glmnet') %>%
# # #         finalize_workflow(parameters = best_tunes %>%
# # #                                   filter(wflow_id == 'preproc_glmnet')) %>%
# # #         last_fit(split = valid_split,
# # #                  metrics = tune_metrics)
# # # 
# # # # glmnet
# # # glmnet_last_fit = workflow_set_rs %>%
# # #         extract_workflow(id = 'preproc_glmnet') %>%
# # #         finalize_workflow(parameters = best_tunes %>%
# # #                                   filter(wflow_id == 'preproc_glmnet')) %>%
# # #         last_fit(split = valid_split,
# # #                  metrics = tune_metrics)
# # # 
# # # # coefs
# # glmnet_last_fit %>%
# #         extract_fit_parsnip() %>%
# #         tidy() %>%
# #         arrange(desc(estimate)) %>%
# #         filter(term!='(Intercept)'
# # 
# # # preds
# # glmnet_last_fit %>%
# #         extract_workflow() %>%
# #         augment(test_games %>%
# #                         filter(.pred_hurdle > .5) %>%
# #                         filter(yearpublished == 2022)) %>%
# #         select(.pred_yes, .pred_hurdle, ever_owned, game_id, name) %>%
# #         mutate_if(is.numeric, round, 3) %>%
# #         arrange(desc(.pred_yes)) 
# # 
# # # coefs
# # glmnet_last_fit %>%
# #         extract_fit_parsnip() %>%
# #         tidy() %>%
# #         arrange(desc(estimate)) %>%
# #         filter(term!='(Intercept)')%>%
# #         filter(estimate !=0) %>%
# #         slice_max(order_by = abs(estimate), n =40) %>%
# #         ggplot(aes(x=estimate, 
# #                    y=reorder(term, estimate)))+
# #         geom_point()
# #         extract_workflow() %>%
# #         augment(test_games %>%
# #                         filter(.pred_hurdle > .5) %>%
# #                         filter(yearpublished == 2023)) %>%
# #         select(.pred_yes, .pred_hurdle, ever_owned, game_id, name) %>%
# #         mutate_if(is.numeric, round, 3) %>%
# #         arrange(desc(.pred_yes)) 
# # 
# # 
# # xgb_last_fit = workflow_set_rs %>%
# #         extract_workflow(id = 'trees_xgb') %>%
# #         finalize_workflow(parameters = best_tunes %>%
# #                                   filter(wflow_id == 'trees_xgb')) %>%
# #         last_fit(split = valid_split,
# #                  metrics = tune_metrics) 
# # 
# # xgb_last_fit %>%
# #         collect_metrics()
# # 
# last_fits %>%
#         filter(wflow_id == 'preproc_glmnet') %>%
#         pluck(".predictions", 1) %>%
#         arrange(.row) %>%
#         bind_cols(., valid_games %>%
#                           select(game_id, name, yearpublished, .pred_hurdle)) %>%
#         filter(.pred_hurdle > .5) %>%
#         select(game_id, name, yearpublished, .pred_yes, ever_owned) %>%
#         arrange(desc(.pred_yes))
# 
# last_fits %>%
#         filter(wflow_id == 'trees_xgb') %>%
#         pluck(".predictions", 1) %>%
#         arrange(.row) %>%
#         bind_cols(., valid_games %>%
#                           select(game_id, name, yearpublished, .pred_hurdle)) %>%
#         filter(.pred_hurdle > .5) %>%
#         select(game_id, name, yearpublished, .pred_yes, ever_owned) %>%
#         arrange(desc(.pred_yes))
# 
# last_fits %>%
#         filter(wflow_id == 'trees_rf') %>%
#         pluck(".predictions", 1) %>%
#         arrange(.row) %>%
#         bind_cols(., valid_games %>%
#                           select(game_id, name, yearpublished, .pred_hurdle)) %>%
#         filter(.pred_hurdle > .5) %>%
#         select(game_id, name, yearpublished, .pred_yes, ever_owned) %>%
#         arrange(desc(.pred_yes))
# 
# last_fits %>%
#         filter(wflow_id == 'trees_xgb') %>%
#         pluck(".predictions", 1) %>%
#         arrange(.row) %>%
#         bind_cols(., valid_games %>%
#                           select(game_id, name, yearpublished, .pred_hurdle)) %>%
#         filter(.pred_hurdle > .5) %>%
#         select(game_id, name, yearpublished, .pred_yes, ever_owned) %>%
#         arrange(desc(.pred_yes))

# 
# foo = 
#         last_fits %>%
#         filter(wflow_id == 'trees_rf') %>%
#         pluck(".workflow", 1) %>%
#         extract_fit_engine()
# 
# # last_fits %>%
# #         filter(wflow_id == 'trees_xgb') %>%
# #         pluck(".predictions", 1) %>%
# #         arrange(.row) %>%
# #         bind_cols(., test_games %>%
# #                           select(game_id, name, yearpublished, .pred_hurdle)) %>%
# #         filter(.pred_hurdle > .5) %>%
# #         select(game_id, name, yearpublished, .pred_yes, ever_owned) %>%
# #         arrange(desc(.pred_yes))
# 
# 
# 
# 
# # 
# last_fits %>%
#         filter(wflow_id == 'trees_rf') %>%
#         pluck(".workflow", 1) %>%
#         augment(test_games %>%
#                         filter(yearpublished == 2023)) %>%
#         filter(.pred_hurdle > .5) %>%
#         select(.pred_yes, ever_owned, game_id, name) %>%
#         mutate_if(is.numeric, round, 3) %>%
#         arrange(desc(.pred_yes))
# 
# last_fits %>%
#         filter(wflow_id == 'trees_xgb' | wflow_id == 'trees_rf') %>%
#         select(wflow_id, .workflow) %>%
#         mutate(preds = map(.workflow, 
#                           ~ .x %>% 
#                                    augment(test_games %>%
#                                                    filter(yearpublished == 2023)) %>%
#                                    filter(.pred_hurdle > .5) %>%
#                                    select(.pred_yes, ever_owned, game_id, name) %>%
#                                    mutate_if(is.numeric, round, 3) %>%
#                                    arrange(desc(.pred_yes)))
#         ) %>% 
#         select(wflow_id, .workflow, preds) %>%
#         unnest(preds) %>%
#         select(wflow_id, .pred_yes, ever_owned, game_id, name) %>%
#         spread(wflow_id, .pred_yes) %>%
#         ggplot(aes(x=trees_rf,
#                    label = name,
#                    y=trees_xgb))+
#         geom_text(check_overlap = T,
#                   vjust = -1,
#                   size = 2.5)+
#         geom_point()+
#         ggpubr::stat_cor()
# 
# last_fits %>%
#         filter(wflow_id == 'preproc_glmnet') %>%
#         pluck(".workflow", 1) %>%
#         augment(test_games %>%
#                         filter(yearpublished == 2022)) %>%
#         filter(.pred_hurdle > .5) %>%
#         select(.pred_yes, ever_owned, game_id, name) %>%
#         mutate_if(is.numeric, round, 3) %>%
#         arrange(desc(.pred_yes)) %>%
#         view()
# 
# # # # save user model
# # # user_model = xgb_last_fit %>%
# # #         extract_workflow()
# # # 
# # # 
# # # # user board
# # # user_models = pins::board_folder(here::here("models", "user_models"), versioned=T)
# # # vetiver_model = vetiver_model(user_model,
# # #                               model_name = paste(username, end_train_year, outcome, "model", sep="_"))
# # # 
# # # #  description = paste("model trained through", end_train_year, "for", username, "predicting", outcome, "on", Sys.Date()))
# # #                  
# # # vetiver_model %>%
# # #         predict()
# # 
# # 
