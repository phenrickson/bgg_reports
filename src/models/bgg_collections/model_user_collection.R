# model user collection

# data splitting --------------------------------------------------------------

message(paste("training models for", username, "on", "outcome"))

message("creating train/valid split...")

# training set for user model
train_games = 
        games_with_user_collection %>%
        # filter to games published through end train year
        filter(yearpublished <= end_train_year) %>%
        # keep only games that meet conditions
        # have a geek rating (is not null for bayesaverage)
        # the user has rated
        # the user has ever owned
        filter(!is.na(bayesaverage) | (rated != 'no' | ever_owned != 'no')) %>%
        # reorder
        select(username, everything())

# valid
valid_games = 
        games_with_user_collection %>%
        # filter to games published through end train year
        filter(yearpublished > end_train_year & yearpublished <= end_train_year +2) %>%
        # filter to only games above specific hurdle
        filter(.pred_hurdle > .25 | !is.na(bayesaverage)) %>%
        # reorder
        select(username, everything())

# upcoming games; no filtering applied
test_games = 
        games_with_user_collection %>%
        # filter to games published through end train year
        filter(yearpublished > end_train_year+2) %>%
        select(username, everything())

# make splits
# make a split for validating on a specific set of years
valid_split = make_splits(
        list(analysis =
                     seq(nrow(train_games)),
             assessment =
                     nrow(train_games) + seq(nrow(valid_games))),
        bind_rows(train_games,
                  valid_games)
)

# make a second split for the training, validation, and test
test_split = make_splits(
        list(analysis =
                     seq(nrow(train_games) + nrow(valid_games)),
             assessment =
                     nrow(train_games) + nrow(valid_games) + seq(nrow(test_games))),
        bind_rows(train_games,
                  valid_games,
                  test_games)
)


# create recipes ------------------------------------------------------------------

message("creating recipes...")

# basic recipe setup
base_recipe_func = function(data,
                            outcome) {
        
        recipe(x=data) %>%
                # set bgg id variables
                update_role(
                        one_of("game_id",
                               "name",
                               "yearpublished",
                               "image",
                               "thumbnail",
                               "description",
                              # "load_ts",
                               "average",
                               "usersrated",
                               "averageweight",
                               "bayesaverage"),
                        new_role = "id") %>%
                # set bgg collection id variables
                update_role(
                        one_of("username",
                                "rating",
                               "rated",
                               "own",
                               "ever_owned",
                               "preordered",
                               "prevowned",
                               "fortrade",
                               "want",
                               "wanttoplay",
                               "wanttobuy",
                               "wishlist",
                               "wishlistpriority"
                               #,
                            #   "username_load_ts"
                               ),
                        new_role = "id") %>%
                # set prediction from hurdle model as an id
                update_role(
                        one_of(".pred_hurdle"),
                        new_role = "id") %>%
                # set outcome varable
                update_role(all_of(outcome),
                            new_role = "outcome") %>%
                # set all others as predictors
                update_role(-has_role("id"),
                            -has_role("outcome"),
                            new_role = "predictor") %>%
                # remove zero variance predictors
                step_zv(all_predictors())
        
}

# main sets of preprocessing
preproc_recipe_func = function(recipe) {
        
        recipe %>%
                # specific for unmatched
                step_mutate(fam_unmatched_series = dplyr::case_when(grepl("Unmatched:", name) ~ 1,
                                                                    TRUE ~ 0)) %>%
                # indicator for no mechanics
                step_mutate(no_mechanics = dplyr::case_when(number_mechanics == 0 ~ 1,
                                                            TRUE ~ 0)) %>%
                step_mutate(no_categories = dplyr::case_when(number_categories == 0 ~ 1,
                                                             TRUE ~ 0)) %>%
                # make time per player variable
                step_mutate(time_per_player = playingtime/ maxplayers) %>% 
                # impute missingness in selected features with median
                step_impute_median(playingtime,
                                   minplayers,
                                   maxplayers,
                                   minage,
                                   time_per_player) %>% # medianimpute numeric predictors
                # truncate minage
                step_mutate(minage = dplyr::case_when(minage > 18 ~ 18,
                                                      minage < 0 ~ 0,
                                                      TRUE ~ minage)) %>%
                # truncate player counts
                step_mutate(minplayers = dplyr::case_when(minplayers > 10 ~ 10,
                                                          TRUE ~ minplayers)) %>%
                step_mutate(maxplayers = dplyr::case_when(maxplayers > 20 ~ 10,
                                                          maxplayers <=1 ~ 1,
                                                          TRUE ~ maxplayers)) %>%
                # remove
                step_rm(minplaytime, maxplaytime) %>%
                # log time per player and playingtime
                step_log(time_per_player,
                         playingtime,
                         offset = 1) %>%
                # truncate yearpublished
                step_mutate(year = dplyr::case_when(yearpublished < 1900 ~ 1900,
                                                    TRUE ~ yearpublished),
                            role = "predictor")
}

# add splines for nonlinearities
splines_recipe_func = function(recipe) {
        
        recipe %>%
                # # then, add spline for truncated yearpublished
                step_ns(year,
                        deg_free = 5) %>%
                # ### nonlinear effects
                step_ns(averageweight,
                        deg_free = 4) %>%
                # # spline for number mechanics
                step_ns(number_mechanics,
                        deg_free = 4)  %>%
                # # spline for number categories
                step_ns(number_categories,
                        deg_free = 4)  %>%
                # spline for min age
                step_ns(minage,
                        deg_free = 3)
                
}

# interactions
# interactions_recipe_func = function(recipe) {
#         
#         recipe %>%
#                 # mechanics and categories
#                 step_interact(~ starts_with("number_mechanics"):starts_with("number_categories")) %>%
#                 # mechanics and time per player
#                 step_interact(~ starts_with("number_mechanics"):time_per_player)
#         
# }


# normalize predictors
norm_recipe_func = function(recipe) {
        
        recipe %>%
                # step_center(all_numeric_predictors()) %>%
                # step_scale(all_numeric_predictors(),
                #            factor = 2)
                 step_normalize(all_numeric_predictors())
        
}

# tune over games to remove via hurdling
downsample_recipe_func = function(recipe,
                                  outcome) {
        
        recipe %>%
                themis::step_downsample(!!outcome, under_ratio = tune::tune())
        
}




# create_folds ------------------------------------------------------------

# create folds based on outcome
create_train_folds = function(data, outcome, v= 5) {
        
        set.seed(1999)
        vfold_cv(data,
                 v = 5,
                 strata = all_of(outcome))
        
}


# models and recipes ------------------------------------------------------


# base logit
glm_class_spec = 
        logistic_reg()

# penalized logistic regression via glmnet
glmnet_class_spec = 
        logistic_reg(penalty = tune::tune(),
                     mixture = 0.5) %>%
        set_engine("glmnet")

# regularization
glmnet_grid = 
        grid_regular(
                penalty(range = c(-5, -.5)),
                levels = 10
        )


# cart for classification
cart_class_spec <-
        decision_tree(
                cost_complexity = tune(),
                tree_depth = tune(),
                min_n = tune()
        ) %>%
        set_mode("classification") %>%
        set_engine("rpart")

# xgb for class
xgb_class_spec <-
        boost_tree(
                trees = 500,
                min_n = tune(),
                sample_size = tune(),
                learn_rate = tune(),
                tree_depth = tune(),
                stop_iter = 50
        ) %>%
        set_mode("classification") %>%
        set_engine("xgboost",
                   eval_metric = 'logloss')

# random forest
rf_class_spec = 
        rand_forest(trees = 500,
                    mtry = tune()) %>%
        set_mode("classification") %>%
        set_engine("ranger")

# lightgbm
library(bonsai)
lightgbm_class_spec <-
        parsnip::boost_tree(
                mode = "classification",
                trees = 500,
                min_n = tune(),
                tree_depth = tune(),
        ) %>%
        set_engine("lightgbm", objective = "binary")

# create tuning grid
# tune_grid =
#         grid_regular(
#                 penalty(range = c(-4, -.25)),
#                 freq_cut(range = c(100, 1000)),
#                 unique_cut(range = c(10, 10)),
#                 levels = c(penalty = 10,
#                            freq_cut = 4,
#                            unique_cut = 1)
#         )


# metrics -----------------------------------------------------------------

class_metrics = metric_set(yardstick::mcc,
                           yardstick::kap,
                           yardstick::precision,
                           yardstick::recall,
                           yardstick::j_index,
                           yardstick::bal_accuracy)

tune_metrics = metric_set(yardstick::mn_log_loss,
                          yardstick::roc_auc)

# create recipes ----------------------------------------------------------


# create a recipe for linear models (normalization, splines)
preproc_recipe = base_recipe_func(data = train_games,
                                       outcome =  outcome) %>%
        # basic preprocessing
        preproc_recipe_func() %>%
        # interactions
      #  interactions_recipe_func() %>%
        # splines
        splines_recipe_func() %>%
        # normalization
        norm_recipe_func() %>%
        check_missing(all_predictors())

# create a recipe for linear model with correlation filter
corr_recipe = preproc_recipe %>%
        # correlation filter
        step_corr(all_predictors(),
                  threshold = 0.9) %>%
        # near zero variance
        step_nzv(starts_with("mec_"),
                 starts_with("cat_"),
                 starts_with("pub_"),
                 starts_with("fam_"),
                 freq_cut = 100) %>%
        check_missing(all_predictors())

# create a recipe for mars (nromalization, no interactions)
norm_recipe = base_recipe_func(data = train_games,
                               outcome =  outcome) %>%
        # basic preprocessing
        preproc_recipe_func() %>%
        # normalization
        norm_recipe_func() %>%
        check_missing(all_predictors())

# create a recipe for trees
trees_recipe = base_recipe_func(data = train_games,
                                  outcome =  outcome) %>%
        # basic preprocessing
        preproc_recipe_func() %>%
        check_missing(all_predictors())

                          
# create workflows --------------------------------------------------------

message("creating workflows...")

# workflow set
user_wflow_set =
        workflow_set(
                preproc = list(
                        preproc = preproc_recipe,
                        corr = corr_recipe,
                        trees = trees_recipe,
                        trees = trees_recipe,
                        trees = trees_recipe,
                        trees = trees_recipe),
                models = list(
                        glmnet = glmnet_class_spec,
                        glm = glm_class_spec,
                        xgb = xgb_class_spec,
                        cart = cart_class_spec,
                        rf = rf_class_spec,
                        lightgbm = lightgbm_class_spec),
                cross = F
        )

# training control --------------------------------------------------------


set.seed(1999)
user_folds = create_train_folds(train_games, outcome)

# tuning control
tune_ctrl = 
        control_grid(
                save_pred = TRUE,
                allow_par = T,
                parallel_over = "resamples",
                verbose = TRUE,
                save_workflow = T,
                event_level = 'second'
        )

# control for racing
race_ctrl = 
        finetune::control_race(
                save_pred = TRUE,
                parallel_over = "everything",
                allow_par = T,
                verbose = TRUE,
                verbose_elim = TRUE,
                save_workflow = F,
                event_level = 'second'
        )

# # control for sim anneal
# sim_ctrl = 
#         finetune::control_sim_anneal(
#                 save_pred = T,
#                 verbose_iter = T,
#                 verbose = T,
#                 event_level = 'second',
#                 parallel_over = 'everything',
#                 allow_par = T
#         )
                


# training ----------------------------------------------------------------

# set parallel
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)


# system.time({
#         lightgbm_rs =
#                 user_wflow_set %>%
#                 extract_workflow(id = "trees_lightgbm") %>%
#                 finetune::tune_race_anova(
#                         resamples =   user_folds,
#                         metrics = tune_metrics,
#                         control = race_ctrl
#                 )
# })


# glm_fit = 
#         glm_wflow %>%
#         fit(train_games)
# 
# glmnet_fit = 
#         glmnet_wflow %>%
#         finalize_workflow(parameters = tibble("penalty" = 0.001)) %>%
#         fit(train_games)

# tune workflow set via race anova
#system.time({
tictoc::tic("training workflows...")
set.seed(2020)
workflow_set_rs <- user_wflow_set %>%
        workflow_map(
                fn = 'tune_race_anova',
                resamples = user_folds,
                metrics = tune_metrics,
                control = race_ctrl,
                grid = 10
        )
tictoc::toc()
#})

# # sim anneal
# system.time({
#         set.seed(2020)
#         workflow_set_rs <- user_wflow_set %>%
#                 workflow_map(
#                         fn = 'tune_sim_anneal',
#                         resamples = user_folds,
#                         metrics = tune_metrics,
#                         control = sim_ctrl,
#                         iter = 10
#                 )
# })

# # tune glmnet race anova
# system.time({
#         set.seed(2020)
#         user_glmnet_rs <- user_glmnet_wflow %>%
#                 finetune::tune_race_anova(
#                         user_folds,
#                         metrics = tune_metrics,
#                         control = race_ctrl,
#                         grid = glmnet_grid
#                 )
# })

# # tune xgb race anova
# system.time({
#         set.seed(2020)
#         user_xgb_rs <- user_xgb_wflow %>%
#                 finetune::tune_race_anova(
#                         user_folds,
#                         metrics = tune_metrics,
#                         control = race_ctrl
#                 )
# })



# results -----------------------------------------------------------------

message("ranking workflows from resampling...")

workflow_set_rs %>%
        rank_results(select_best = T) %>%
        ggplot(aes(x=mean,
                   xmin = mean - 1.96*std_err,
                   xmax = mean + 1.96*std_err,
                   y = wflow_id))+
        facet_wrap(~ .metric,
                   scales = "free")+
        geom_pointrange()+
        theme_bw()

workflow_set_rs %>%
        rank_results(select_best = T) %>%
        select(rank, wflow_id, model, .metric, mean, std_err, n)


# predictions -------------------------------------------------------------

message("collecting predictions from resampling...")

preds_rs = 
        workflow_set_rs %>%
        collect_predictions(select_best = 1, metric = 'mn_log_loss') %>%
        left_join(.,
                  train_games %>%
                          mutate(.row = row_number()) %>%
                          select(.row, game_id, name, yearpublished)) %>%
        select(.row, wflow_id, game_id, name, yearpublished, .pred_yes, !!outcome)

# preds_rs %>%
#         filter(wflow_id == 'trees_xgb') %>%
#         arrange(desc(.pred_yes)) %>%
#         print(n = 25)
#         
# 
# preds_rs %>%
#         filter(wflow_id == 'preproc_glmnet') %>%
#         arrange(desc(.pred_yes)) %>%
#         print(n = 25)
#         
        # spread(wflow_id, .pred_yes) %>%
        # arrange(desc(preproc_glmnet)) %>%
        # mutate(ensemble = (preproc_glmnet + trees_xgb) / 2) %>%
        # arrange(desc(ensemble))
# best tune ---------------------------------------------------------------

tune_metric = 'mn_log_loss'

message("selecting best tuning parameters via ", tune_metric)

best_tunes = 
        workflow_set_rs %>%
        mutate(best_tune = map(result, select_best, metric = 'mn_log_loss', n=1))  %>%
        select(wflow_id, best_tune) %>%
        unnest(best_tune)

# # # get best tune
# # best_glmnet_tune = 
# #         user_glmnet_rs %>%
# #         select_best(metric = 'roc_auc')
# # 
# # best_xgb_tune = 
# #         user_xgb_rs %>%
# #         select_best(metric = 'roc_auc')
# # 
# 
# user_glmnet_rs %>%
#         collect_metrics(parameters = best_glmnet_tune, select_best = T) %>%
#         filter(penalty == best_glmnet_tune$penalty) %>%
#         mutate(method = 'glmnet') %>%
#         bind_rows(.,
#                   user_xgb_rs %>%
#                           collect_metrics(parameters = best_xgb_tune, select_best = T) %>%
#                           mutate(method = 'xgb')) %>%
#         select(method, .metric, mean, n, std_err) %>%
#         arrange(.metric)
# 
# 
# user_glmnet_rs %>%
#         collect_predictions(parameters = best_glmnet_tune, select_best = T) %>%
#         mutate(method = 'glmnet') %>%
#         bind_rows(.,
#                   user_xgb_rs %>%
#                           collect_predictions(parameters = best_xgb_tune, select_best = T) %>%
#                           mutate(method = 'xgb')) %>%
#         select(method, id, .pred_yes, .row, ever_owned) %>%
#         left_join(.,
#                   train_games %>%
#                           select(game_id, name, yearpublished) %>%
#                           mutate(.row = row_number())) %>%
#         spread(method, .pred_yes) %>%
#         ggplot(aes(x=glmnet,
#                    label = name,
#                    y=xgb))+
#         geom_point(aes(color = ever_owned))+
#         geom_text(check_overlap = T,
#                   vjust = -1,
#                   size = 2.5)+
#         coord_cartesian(xlim = c(0, 1),
#                         ylim = c(0, 1))
#         
# 
# # collect predictions
# preds = user_rs %>%
#         collect_predictions(parameters = best_tune) %>%
#         left_join(.,
#                   train_games %>%
#                           mutate(.row = row_number()) %>%
#                           select(.row, game_id, name, yearpublished),
#                   by = c(".row")) %>%
#         select(id, .row, .pred_yes, ever_owned, game_id, name, yearpublished) %>%
#         arrange(desc(.pred_yes))
# 
# # threshold 
# map(seq(0, 1, 0.02),
#     ~ preds %>%
#             mutate(.pred_class = factor(case_when(.pred_yes > .x ~ 'yes',
#                                            TRUE ~ 'no'),
#                                         levels = c("no", "yes"))) %>%
#             mutate(.threshold = .x) %>%
#             group_by(.threshold) %>%
#             class_metrics(truth = ever_owned,
#                           estimate = .pred_class,
#                           event_level = 'second')) %>%
#         bind_rows() %>%
#         ggplot(aes(x=.threshold,
#                    y = .estimate))+
#         geom_line()+
#         facet_wrap(~.metric)+
#         theme_bw()
# 
# # precision recall curve
# user_glmnet_rs %>%
#         collect_predictions(parameters = best_glmnet_tune, select_best = T) %>%
#         mutate(method = 'glmnet') %>%
#         bind_rows(.,
#                   user_xgb_rs %>%
#                           collect_predictions(parameters = best_xgb_tune, select_best = T) %>%
#                           mutate(method = 'xgb')) %>%
#         group_by(method) %>%
#         yardstick::roc_curve(ever_owned,
#                              .pred_yes,
#                              event_level = 'second') %>%
#         autoplot()+
#         theme_bw()
# 
preds_rs %>% 
        group_by(wflow_id) %>%
        yardstick::pr_curve(ever_owned,
                             .pred_yes,
                             event_level = 'second') %>%
        autoplot()+
        theme_bw()+
        facet_wrap(wflow_id ~.)+
        guides(color = 'none')

preds_rs %>%
        group_by(wflow_id) %>%
        yardstick::lift_curve(ever_owned,
                            .pred_yes,
                            event_level = 'second') %>%
        autoplot()+
        theme_bw()+
        facet_wrap(wflow_id ~.)+
        guides(color = 'none')

preds_rs %>%
group_by(wflow_id) %>%
        yardstick::roc_curve(ever_owned,
                              .pred_yes,
                              event_level = 'second') %>%
        autoplot()+
        theme_bw()+
        scale_color_viridis_d()
# 
# 
# # calibration
preds_rs %>%
        group_by(wflow_id, prob = plyr::round_any(.pred_yes, .05),
                 ever_owned) %>%
        count() %>%
        group_by(wflow_id, prob) %>%
        mutate(prop = n / sum(n)) %>%
        mutate(total = sum(n)) %>%
        filter(ever_owned == 'yes') %>%
        ggplot(aes(x=prob,
                   group = wflow_id,
                   size = total,
                   y = prop))+
        geom_point()+
        geom_line(lwd = 0.5)+
        coord_obs_pred()+
        geom_abline()+
        theme_minimal()+
        facet_wrap(wflow_id~.)
# 
# # 
preds_rs %>%
        group_by(wflow_id) %>%
        probably::cal_plot_windowed(ever_owned,
                                    .pred_yes,
                                  window_size = 0.1,
                                    event_level = 'second')+
        facet_wrap(wflow_id ~.)

# user_glmnet_rs %>%
#         collect_predictions(parameters = best_glmnet_tune, select_best = T) %>%
#         mutate(method = 'glmnet') %>%
#         bind_rows(.,
#                   user_xgb_rs %>%
#                           collect_predictions(parameters = best_xgb_tune, select_best = T) %>%
#                           mutate(method = 'xgb'))   %>%
#         group_by(method) %>%
#         probably::cal_plot_logistic(ever_owned, 
#                                   .pred_yes,
#                                   window.size = .05,
#                                   event_level = 'second')
# 
preds_rs %>%
        group_by(wflow_id) %>%
        probably::threshold_perf(ever_owned, .pred_yes, seq(0, 1, by = 0.1),
                       event_level = 'second') %>%
        filter(.metric != 'distance') %>%
        ggplot(aes(x=.threshold,y=.estimate, color = .metric))+
        geom_line()+
        scale_color_viridis_d() +
        theme_bw()+
        facet_wrap(wflow_id ~.)
# 
# # examine
# preds %>%
#         arrange(.row) %>%
#         left_join(.,
#                   train_games %>%
#                           select(game_id, name, yearpublished) %>%
#                           mutate(.row = row_number())) %>%
#         select(game_id, name, yearpublished, .pred_yes, ever_owned) %>%
#         arrange(desc(.pred_yes))
# 
# # fit
# user_last_xgb_fit = 
#         user_xgb_wflow %>%
#         finalize_workflow(parameters = best_xgb_tune) %>%
#         last_fit(split = valid_split,
#                  metrics = tune_metrics)
# 
# user_last_glmnet_fit = 
#         user_glmnet_wflow %>%
#         finalize_workflow(parameters = best_glmnet_tune) %>%
#         last_fit(split = valid_split,
#                  metrics = tune_metrics)
# 
# # 
# user_last_fit %>%
#         extract_workflow() %>%
#         extract_fit_parsnip() %>%
#         tidy() %>%
#         filter(estimate != 0) %>%
#         filter(term != '(Intercept)') %>%
#         #filter(!grepl("_ns_", term)) %>%
#       #  group_by(sign = case_when(estimate > 0 ~ 'positive', estimate < 0 ~ 'negative')) %>%
#         slice_max(order_by = abs(estimate), n =40, with_ties = F) %>%
#         ggplot(aes(x=estimate,
#                    y=reorder(term, estimate)))+
#         geom_point()+
#         geom_vline(xintercept = 0)
# 
# user_last_fit %>%
#         collect_predictions() %>%
#         tune_metrics(truth = ever_owned,
#                       estimate = .pred_class,
#                      .pred_yes,
#                       event_level = 'second')
# 
# user_last_fit %>%
#         collect_predictions() %>%
#         arrange(desc(.row)) %>%
#         left_join(.,
#                    valid_games %>%
#                           mutate(.row = nrow(train_games)+row_number()) %>%
#                           select(game_id, yearpublished, bayesaverage, name, .row)) %>%
#         filter(!is.na(bayesaverage)) %>%
#         select(id, .pred_yes, ever_owned, game_id, name, yearpublished) %>%
#         arrange(desc(.pred_yes))
# # 
# user_last_fit %>%
#         pluck(".workflow", 1) %>%
#         predict(test_games %>%
#                         mutate(love = 'yes'),
#                 type = 'prob') %>%
#         bind_cols(., test_games %>%
#                           select(game_id, name, ever_owned, yearpublished, .pred_hurdle)) %>%
#         filter(yearpublished == 2022) %>%
#         filter(.pred_hurdle > .25) %>%
#         arrange(desc(.pred_yes)) %>%
#         mutate(rank = row_number()) %>%
#         mutate_if(is.numeric, round, 2) %>%
#         view()
