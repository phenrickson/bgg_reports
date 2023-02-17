# what: train a hurdle model to predict whether a game will receive a geek rating 
# aka achieve a user threshold rating

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
        
        # vetiver
        library(vetiver)
        
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

# functions used in creating features fore games
source(here::here("src", "features", "make_features_functions.R"))

# for plotting
source(here::here("src", "helpers", "theme_phil.R"))


# data splitting -----------------------------------------------------------

# use tidy and split games function to designate train test split

# get filtered games given training year
tidied_games = 
        # apply simple preprocessing
        tidy_games(analysis_games) %>%
        # replace NA in usersrated with 0
        mutate(usersrated = replace_na(usersrated, 0)) %>%
        # convert yearpublished and usersrated to numeric
        mutate_at(c("usersrated",
                    "yearpublished"),
                  as.numeric) %>%
        # add description
        left_join(., game_descriptions %>%
                          select(game_id, description),
                  by = c("game_id")) %>%
        # features to keep
        transmute(game_id,
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
                  load_ts) %>%
        # create binary outcome for threshold
        mutate(users_threshold = factor(case_when(!is.na(bayesaverage) ~ 'yes',
                                                  TRUE ~ 'no'),
                                        levels = c("no", "yes")))

# visualize the number of games that get above this threshold by year
# tidied_games %>%
#         filter(yearpublished < 2020) %>%
#         mutate(yearpublished = case_when(yearpublished < 1900 ~ 1900,
#                                 TRUE ~ yearpublished)) %>%
#         group_by(yearpublished, users_threshold) %>%
#         count() %>%
#         ungroup() %>%
#         # spread(users_threshold, n) %>%
#         # mutate(prop = yes / no) %>%
#         # mutate(prop = replace_na(prop, 0)) %>%
#         ggplot(aes(x=yearpublished,
#                    fill = users_threshold,
#                    y=n))+
#         geom_area(position = 'fill')

# training set for hurdle model
hurdle_train =
        hurdle_prep(tidied_games %>%
                            filter(yearpublished <= end_train_year))

# valid set for hurdle model
hurdle_valid = 
        hurdle_prep(tidied_games %>%
                            filter(yearpublished > end_train_year))
        
# make recipes for hurdle
base_recipe_func = function(data,
                            outcome) {
        
        recipe(x=data) %>%
                # set id variables
                update_role(
                        one_of("game_id",
                               "name",
                               "yearpublished",
                               "image",
                               "thumbnail",
                               "description",
                               "load_ts",
                               "average",
                               "usersrated",
                               "users_threshold",
                               "averageweight",
                               "bayesaverage"),
                        new_role = "id") %>%
                # set outcome varable
                update_role(all_of(outcome),
                            new_role = "outcome") %>%
                # set all others as predictors
                update_role(-has_role("id"),
                            -has_role("outcome"),
                            new_role = "predictor") %>%
                # some simple feature engineering
                # number of mechanics
                step_mutate(number_mechanics = rowSums(dplyr::across(starts_with("mec_")))) %>%
                # mumber of categories
                step_mutate(number_categories = rowSums(dplyr::across(starts_with("cat_")))) %>%
                # solo game
                # big box/deluxe/anniversary edition
                step_mutate(deluxe_edition = dplyr::case_when(grepl("kickstarter|big box|deluxe|mega box", tolower(name))==T ~ 1,
                                                       TRUE ~ 0)) %>%
                # word count
                step_mutate(word_count = stringi::stri_count_words(description)) %>%
                step_mutate(word_count = tidyr::replace_na(word_count, 0)) %>%
                # magical phrase in description
                step_mutate(description_from_publisher = dplyr::case_when(grepl("description from publisher", tolower(description))==T ~ 1,
                                                                   TRUE ~ 0)) %>%
                # missingness
                step_mutate(missing_minage = dplyr::case_when(is.na(minage) ~ 1,
                                                       TRUE ~ 0)) %>%
                step_mutate(missing_playingtime = dplyr::case_when(is.na(playingtime) ~ 1,
                                                            TRUE ~ 0)) %>%
                # remove zero variance
                step_zv(all_predictors())
        
}

# additional preprocessing
# impute missigness on playing time via median
preproc_recipe_func = function(recipe) {
        
        recipe %>%
        step_impute_median(playingtime,
                   minplayers,
                   maxplayers,
                   minage) %>% # medianimpute numeric predictors
        # truncate player counts
        step_mutate(minplayers = dplyr::case_when(minplayers > 10 ~ 10,
                                                  TRUE ~ minplayers)) %>%
        step_mutate(maxplayers = dplyr::case_when(maxplayers > 20 ~ 10,
                                                  maxplayers <=1 ~ 1,
                                                  TRUE ~ maxplayers)) %>%
        # remove
        step_rm(minplaytime, maxplaytime) %>%
        # make time per player variable
        step_mutate(time_per_player = playingtime/ maxplayers) %>% 
        # log time per player and playingtime
        step_log(time_per_player,
                 playingtime,
                 offset = 1) %>%
        # normalize
        step_normalize(all_numeric_predictors())
}


# models ------------------------------------------------------------------


# xgboost
xgb_spec <-
        boost_tree(
                trees = 250,
                #  sample_size = tune(),
                min_n = tune(),
                mtry = tune(), # randomness
                learn_rate = tune(),  
                tree_depth = tune(),
                stop_iter = 50) %>%
        set_mode("classification") %>%
        set_engine("xgboost",
                   eval_metric = 'logloss',
                   counts = F) # for mtry to be in [0,1]

# # set up grid for tuning
xgb_grid = 
        grid_latin_hypercube(
                tree_depth = tree_depth(range = c(2L, 9L)),
                min_n(),
                #   sample_size = sample_prop(),
                mtry = mtry_prop(c(0.1, 1)),
                learn_rate(),
                size = 15
        )

# penalized logistic regression
glmnet_spec <- 
        logistic_reg(mode = "classification",
                     penalty = tune::tune(),
                     mixture = 0.5) %>%
        set_engine("glmnet")

# specify grid for tuning glmnet
glmnet_grid <- 
        glmnet_spec %>%
        extract_parameter_set_dials() %>%
        update(penalty = penalty(range = c(-5, -0.5)))

# classification metrics
# probabilities
prob_metrics = metric_set(yardstick::mn_log_loss,
                          yardstick::roc_auc)

# class
class_metrics = metric_set(yardstick::accuracy,
                           yardstick::bal_accuracy,
                           yardstick::mcc,
                           yardstick::kap,
                           yardstick::j_index,
                           yardstick::f_meas,
                           yardstick::precision,
                           yardstick::recall,
                           yardstick::ppv)

# create resamples with 5 fold repeated cross validation
set.seed(1999)
hurdle_folds = vfold_cv(hurdle_train,
                        v = 5,
                        strata = users_threshold)

# create wflow
hurdle_wflows = 
        workflow_set(
                # recipe with preprocessing/imputation
                preproc = list( 
                        # use onlybase recipe, little preprocessing
                        base = 
                                base_recipe_func(hurdle_train,
                                                 "users_threshold"),
                        # imputation and splines for glmnet
                        preproc =
                                base_recipe_func(hurdle_train,
                                                 "users_threshold") %>%
                                preproc_recipe_func()),
                # model specifications
                models =
                        list(xgb = xgb_spec,
                             glmnet = glmnet_spec),
                cross = F) %>%
        # keep selected wflows
        # xgb grid
        option_add(grid = xgb_grid, id = c("base_xgb")) %>%
        option_add(param_info = glmnet_grid, id = c("preproc_glmnet"))

# set parallelization
all_cores <- parallel::detectCores(logical = FALSE)-1
doMC::registerDoMC(cores = all_cores)

# control for racing
race_ctrl <-
        finetune::control_race(
                save_pred = TRUE,
                parallel_over = "resamples",
                event_level = 'second',
                verbose = TRUE,
                verbose_elim = TRUE,
                save_workflow = TRUE
        )

# get results of tuning
race_res =
        hurdle_wflows %>%
        workflow_map(
                "tune_race_anova",
                seed = 1999,
                resamples = hurdle_folds,
                control = race_ctrl,
                metrics = prob_metrics
        )

# collect metrics
hurdle_metrics = race_res %>%
        rank_results(select_best = T, rank_metric = 'mn_log_loss')

# select best model based on log loss
best_mod = hurdle_metrics %>%
        filter(.metric == 'mn_log_loss') %>%
        slice_min(rank, n =1, with_ties = F) %>%
        pull(wflow_id) %>%
        unique

# gather predictions
hurdle_preds = race_res %>%
        collect_predictions(select_best = T, metric = 'mn_log_loss')

# assess cutpoint for classification metrics
assess_cutpoint = 
        map(seq(0.1, 0.9, 0.01),
            ~ hurdle_preds %>%
                    mutate(.pred_class = factor(case_when(.pred_yes > .x ~ 'yes',
                                                          TRUE ~ 'no'),
                                                levels = c("no", "yes"))) %>%
                    mutate(cutpoint = .x) %>%
                    group_by(wflow_id, cutpoint) %>%
                    class_metrics(truth = users_threshold,
                                           estimate = .pred_class)) %>%
        bind_rows

# examine estimates across cutpoint
assess_cutpoint %>%
        ggplot(aes(x=cutpoint,
                   color = wflow_id,
                   y=.estimate))+
        geom_line(lwd = 1.04)+
        facet_wrap(.metric ~.,
                   scales = "free_y")+
        theme_phil()+
        scale_color_viridis_d()

# apply a custom cost function
# predicting that game will not meet the users threshold when it does is far more costly than the opposite
# in this case, the cost of a false negative is 10 times more worse than the cost of a false positive
assess_costs = 
        map(seq(0, 0.9, 0.01),
            ~ hurdle_preds %>%
                    mutate(.pred_class = factor(case_when(.pred_yes > .x ~ 'yes',
                                                          TRUE ~ 'no'),
                                                levels = c("no", "yes"))) %>%
                    mutate(cutpoint = .x) %>%
                    mutate(cost = case_when(users_threshold == .pred_class ~ 0,
                                            users_threshold == 'yes' & .pred_class == 'no' ~ 10,
                                            users_threshold == 'no' & .pred_class == 'yes' ~ 1)) %>%
                    group_by(wflow_id, cutpoint) %>%
                    summarize(cost = sum(cost),
                              .groups = 'drop'))%>%
        bind_rows

# plot cutpoint
assess_costs %>% {
        
        ggplot(.,
               aes(x=cutpoint,
                   color = wflow_id,
                   y = cost))+
                geom_line(lwd = 1.04)+
                theme_phil()+
                scale_color_viridis_d()+
                geom_vline(xintercept = 
                                   slice_min(., cost, n= 1, with_ties = F) %>%
                                   pull(cutpoint))+
                scale_y_continuous()
}


# get cutpoint
cutpoint = assess_costs %>%
        group_by(wflow_id) %>%
        slice_min(cost, n = 1, with_ties = F) %>%
        filter(wflow_id == best_mod) %>%
        pull(cutpoint)

# confusion matrix
hurdle_preds %>%
        filter(wflow_id == best_mod) %>%
        mutate(.pred_class = factor(case_when(.pred_yes > cutpoint ~ 'yes',
                                              TRUE ~ 'no'),
                                    levels = c("no", "yes"))) %>%
        yardstick::conf_mat(users_threshold,
                            estimate = .pred_class) %>%
        autoplot(type = 'mosaic')

# gather metrics
# metrics = race_res %>%
#         collect_metrics(select_best=T, metric = 'roc_auc')

# get best tune
best_tune = race_res %>%
        mutate(best_tune = map(result,
                               select_best, metric = 'roc_auc')) %>%
        select(wflow_id, best_tune) %>%
        filter(wflow_id == best_mod) %>%
        pluck("best_tune", 1)

# finalize mod
hurdle_fit = 
        race_res %>%
        extract_workflow(id = best_mod) %>%
        finalize_workflow(parameters = best_tune) %>%
        fit(hurdle_train)

# save model with vetiver
model_board = pins::board_folder(here::here("models", "board"))
vetiver_hurdle = vetiver::vetiver_model(model = hurdle_fit,
                                        model_name = "hurdle_model",
                                        description = paste("classifiation model trained through", end_train_year, "to predict whether games will hit user ratings threshold"))

# test that it predicts
testthat::test_that("vetiver model does not error due to package",
                    testthat::expect_no_error(
                            vetiver_hurdle %>% 
                                    predict(hurdle_train %>% 
                                                    sample_n(10)))
)

# store
vetiver::vetiver_pin_write(model_board, vetiver_hurdle)


# library(probably)
# hurdle_preds %>%
#         filter(wflow_id == 'base_xgb') %>%
#         cal_plot_breaks(users_threshold, .pred_yes,
#                         event_level = 'second',
#                         num_breaks = 25)

# # confusion matrix
# last_fit %>% 
#         collect_predictions() %>%
#         mutate(.pred_class = factor(case_when(.pred_yes > cutpoint ~ 'yes',
#                                               TRUE ~ 'no'),
#                                     levels = c("no", "yes"))) %>%
#         yardstick::conf_mat(truth = users_threshold,
#                             estimate = .pred_class) %>%
#         autoplot(type = 'heatmap')+
#         ggtitle("Confusion Matrix for Validation Set",
#                 subtitle = paste('xgboost with cutpoint at', cutpoint))
                
# get preds for validation set
# valid_preds = last_fit %>% 
#         collect_predictions() %>%
#         mutate(.pred_class = factor(case_when(.pred_yes > cutpoint ~ 'yes',
#                                               TRUE ~ 'no'),
#                                     levels = c("no", "yes")))

# # examine
# valid_preds %>%
#         select(.pred_yes, .row, .pred_class, users_threshold) %>%
#         bind_cols(., valid %>% 
#                           select(game_id, name, usersrated)) %>%
#         arrange(.pred_yes) %>% 
#         mutate_if(is.numeric, round, 3)  %>%
#         filter(.pred_class == 'no') %>%
#         arrange(desc(usersrated))
# 
# # plot probs
# valid_preds %>%
#         ggplot(aes(x=.pred_yes,
#                    fill = users_threshold))+
#         geom_density(alpha = 0.8)+
#         facet_wrap(users_threshold ~., ncol = 1)+
#         theme_phil()+
#         theme(legend.title = element_text())
        
# # get results on validation set
# hurdle_wflow = last_fit %>% 
#         extract_workflow() %>%
#         finalize_workflow(parameters = best_tune) %>%
#         fit(hurdle_train)

# pin workflow


# ## shapley
# 
# # extract mold
# mold = wflow %>%
#         extract_mold()
# 
# # extract model
# mod = wflow %>%
#         extract_fit_engine()
# 
# # extract recipe template
# template = wflow %>%
#         extract_preprocessor() %$%
#         template
# 
# # extract training set
# orig = 
#         bind_cols(
#                 # outcome
#                 wflow %>%
#                         extract_mold() %$%
#                         outcomes,
#                 # predictors
#                 wflow %>%
#                         extract_mold() %$%
#                         predictors,
#                 # ids
#                 wflow %>%
#                         extract_mold() %$%
#                         extras %$%
#                         roles %$%
#                         id) %>%
#         select(names(template))
# 
# # get outcome var
# outcome =   wflow %>%
#         extract_mold() %$%
#         outcomes
# 
# # get list of features used in model
# features =
#         mod %$%
#         feature_names
# 
# # matrix of features used in model
# mat = wflow %>% 
#         extract_mold() %$% 
#         predictors %>%
#         as.matrix()
# 
# # bake new
# new = 
#         wflow %>%
#         extract_preprocessor() %>%
#         prep(strings_as_factor = F) %>%
#         bake(new_data = valid)
# 
# # run shap
# shap = fastshap::explain(mod,
#                          exact = T,
#                          newdata = new %>%
#                                  select(one_of(features)) %>%
#                                  as.matrix) %>%
#         as_tibble()
# 
# shap[323,] %>%
#         pivot_longer(cols = everything()) %>%
#         mutate(value = -value) %>%
#         mutate(sign = case_when(value > 0 ~ 'positive',
#                                   value < 0 ~ 'negative')) %>%
#         filter(value !=0) %>%
#         group_by(sign) %>%
#         slice_max(order_by = abs(value), n = 10) %>% 
#         ggplot(aes(x=value,
#                    fill = value,
#                    y = reorder(name, value)))+
#         geom_col()+
#         geom_vline(xintercept = 0)+
#         ggthemes::scale_fill_gradient2_tableau()
# 
