# wflow = last_fits %>%
#         filter(wflow_id == 'trees_lightgbm') %>%
#         pluck(".workflow", 1)
# 
# data = train_games
# 
# # extract model
# mod = wflow %>%
#         extract_fit_engine()
# 
# # extract rec
# rec = wflow %>%
#         extract_preprocessor()
# 
# # get list of features used in model
# # features =
# #         mod %$%
# #         feature_names
# 
# mold = wflow %>%
#         extract_mold() %$%
#         predictors
# 
# # matrix of features used in model
# 
# mat = wflow %>%
#         extract_mold() %$%
#         predictors %>%
#         as.matrix()
# # 
# pfun <- function(object, newdata) {
#         predict(object, data = newdata)
# }
# 
# pfun(mod, mat)
# 
# expl <- fastshap::explain(mod,
#                           X = mat ,
#                           pred_wrapper = pfun, 
#                           nsim = 10)
# 
# autoplot(expl, type = "contribution")
# 
# # 
# # # run shap
# shap = fastshap::explain(mod,
#                          exact = T,
#                          X = mat)
# 
# shap
#                          
#                          # new_data = mat[1:5,],
#                         # feature_names =
#                        #  X = mat[14233:(14233+1), , drop = F],
#                       #   newdata = mat[14233:(14233+1), , drop = F],
#    #                      pred_wrapper = pfun)
# # 
# # shap[1,] %>% gather() %>% arrange(desc(value))
# # 
# # # get importance
# shap_imp <- data.frame(Variable = names(shap),
#                        Importance = apply(shap,
#                                           MARGIN = 2, FUN = function(x) mean(abs(x)))) %>%
#         as_tibble() %>%
#         arrange(desc(Importance))
# 
# # 
# # # viw all
# # preds = wflow %>%
# #         augment(train_games) %>%
# #         mutate(.row = row_number()) %>%
# #         select(.row, game_id, name, .pred_yes,  ever_owned) %>%
# #         bind_cols(., shap %>%
# #                   as_tibble)
# # 
# # 
# # preds[19399,] %>%
# #         pivot_longer(cols = -c(.row, game_id, name, .pred_yes, outcome),
# #                      names_to = "feature",
# #                      values_to = "value") %>%
# #         mutate(value = -value) %>%
# #         arrange(desc(value)) %>%
# #         slice_max(abs(value), n = 40) %>%
# #         ggplot(aes(x=value,
# #                    y = reorder(feature, value)))+
# #         geom_col()
# # 
# # 
# # 
# # 
# # preds %>%
# #         sample_n(1000) %>%
# #         pivot_longer(cols = -c(game_id, name, .pred_yes, outcome),
# #                      names_to = "feature",
# #                      values_to = "value")
# #         # mutate 
# #         mutate(feature = case_when(feature %in% top_vars$rowname ~ feature,
# #                                    TRUE ~ 'other')) %>%
# #         # reorder
# #         mutate(feature = factor(feature,
# #                                 levels = rev(c(top_vars$rowname, 'other')))) %>%
# #         # median
# #         ggplot(aes(x=value,
# #                    color = value,
# #                    y=feature))+
# #         geom_point(position = ggforce::position_jitternormal(sd_x = 0))+
# #         theme_minimal()
# #         
# #         autoplot(shap, 
# #                  type = "dependence", 
# #                  feature = "minage", 
# #                  exact = T,
# #                  X = mat,
# #                  newdata = mat,
# #                  event_level = 'second',
# #                  smooth = TRUE)+
# #                 scale_y_reverse()
#         
#         
# var = 'number_mechanics'
# range = range(train_games[,var], na.rm = T)
# num = seq(min(range), 30, length.out = 10)
# 
# preds = map(num,
#   ~  wflow %>%
#             augment(train_games %>%
#                             mutate(!!var:= as.integer(.x),
#                                    type = 'prob')) %>%
#           select(.pred_yes, game_id, name, !!var)) %>%
#         bind_rows() 
# 
# column <- ensym(var)
# 
# preds %>%
#         ggplot(aes(x = !!column,
#                    group = game_id,
#                    y = .pred_yes))+
#         geom_line(alpha = 0.25)

