# what: impute missingness in data for averageweight

# dependencies:
# preprocessing.R
# recipes.R
# models.R

# missingness in training set
# train %>%
#         summarise(across(everything(), ~ sum(is.na(.)))) %>%
#         pivot_longer(cols = everything()) %>%
#         arrange(desc(value))
# 
# # viz missingness
# naniar::vis_miss(train %>%
#                          arrange(game_id) %>%
#                          select(-load_ts, -description) %>%
#                          select(-starts_with("pub_"),
#                                 -starts_with("cat_"),
#                                 -starts_with("fam_"),
#                                 -starts_with("des_"),
#                                 -starts_with("mec_"),
#                                 -starts_with("art_")),
#                  warn_large_data = F)


# train a model to predict averageweight
# can do this within the recipe; however, given its importance as a feature i want a specific model

# resampling --------------------------------------------------------------

# remove missingness from train
impute = train %>%
        filter(!is.na(averageweight))

# now split for the purpose of train/valid
set.seed(1999)
impute_split = rsample::initial_split(impute,
                                         prop = 3/4,
                                         strata = averageweight)

# create resamples with 5 fold repeated cross validation
set.seed(1999)
impute_train_folds = vfold_cv(analysis(impute_split),
                       v = 5,
                   #    repeats = 3,
                       strata = averageweight)


# workflow sets -----------------------------------------------------------

# initial recipe
game_creator_rec = base_recipe_func(data = analysis(impute_split),
                              "averageweight") %>%
        # preprocess
        preproc_recipe_func()

# game recipe
# removes publisher, designer, and artist features, as these blow up the dimensionality quite a bit
game_rec = base_recipe_func(data = 
                                       analysis(impute_split) %>%
                                       # remove certain categorical features from the dataset 
                                       # before hitting the recipe
                                       select(-starts_with("pub_"),
                                               -starts_with("des_"),
                                               -starts_with("art_")),
                               "averageweight") %>%
        # preprocess
        preproc_recipe_func()

# create workflow sets for predicting average weight
impute_wflows = 
        workflow_set(
                # recipe with preprocessing/imputation
                preproc = 
                        list(
                                # preproc with all features
                                game_creator = game_creator_rec,
                                # preproc with fewer features
                                game = game_rec,
                                # normalize game_creator
                                game_creator_norm = game_creator_rec %>%
                                        norm_recipe_func(),
                                # normalize without creator features
                                game_norm = game_rec %>%
                                        norm_recipe_func()
                        ),
                # model specifications
                models =
                        list(xgb = xgb_spec,
                             lm = lm_spec,
                             glmnet = glmnet_spec),
                cross = T) %>%
        # keep selected wflows
        # only keep specific combinations of workflows
        inner_join(
                tibble(
                        wflow_id =
                                c("game_xgb",
                                  "game_norm_lm",
                                  "game_creator_norm_glmnet",
                                  "game_norm_glmnet")),
                by = c("wflow_id")) %>%
        # add in custom tuning grids
        # glmnet grid
        option_add(param_info = glmnet_grid, id = c("game_creator_norm_glmnet",
                                                    "game_norm_glmnet")) %>%
        # xgb grid
        option_add(grid = xgb_grid, id = c("game_xgb"))

# tune and evaluate via race
# register parallel
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)

# control for racing
race_ctrl <-
        finetune::control_race(
                save_pred = TRUE,
                parallel_over = "everything",
                verbose = TRUE,
                verbose_elim = TRUE,
                save_workflow = TRUE
        )
              
# get results of tuning  
impute_race_res = 
        impute_wflows %>%
        workflow_map(
                "tune_race_anova",
                seed = 1999,
                resamples = impute_train_folds,
                control = race_ctrl
        )

# now tune and evaluate on train folds
impute_race_res %>%
        rank_results(rank_metric = 'rmse') %>%
        arrange(.metric)

# visualize
impute_race_res %>% 
        collect_predictions(select_best = T, metric = 'rmse') %>% 
        left_join(., assessment(impute_split) %>%
                          mutate(.row = row_number()) %>% 
                          select(.row, game_id, name),
                  by = c(".row")) %>%
        select(wflow_id, .row, averageweight, .pred, game_id, name) %>%
        pivot_wider(names_from = c("wflow_id"),
                    values_from = c(".pred")) %>%
        ggplot(aes(x=.panel_x,
                   color = averageweight,
                   y=.panel_y))+
        ggforce::geom_autopoint(size = 0.5)+
        geom_autodensity(alpha = 0.5)+
        ggforce::facet_matrix(vars(-.row, -game_id, -name),
                              vars(-.row, -game_id, -name),
                              layer.diag=2)+
        theme_bw()+
        scale_color_gradient(low = "blue", high = "red")+
        theme(legend.position = 'top')

# last fit with best tune
impute_last_fits = 
        impute_race_res %>% 
        # get best tune
        mutate(best_tune = map(result,
                               select_best, 
                               metric = 'rmse', 
                               n=1)) %>%
        mutate(last_fit = map2(result,
                            best_tune,
                            ~ .x %>%
                                    extract_workflow %>%
                                    finalize_workflow(parameters = .y) %>%
                                    last_fit(split = impute_split,
                                             metrics = reg_metrics))) %>%
        select(wflow_id, best_tune, last_fit)

# assess performance on validation set that was set aside
impute_last_fits %>%
        unnest(last_fit) %>%
        select(wflow_id, .metrics) %>% 
        unnest(.metrics) %>%
        arrange(.metric)

# fit final workflow
impute_wflows = 
        impute_last_fits %>%
        unnest(last_fit) %>%
        select(wflow_id, best_tune, .workflow) %>%
        mutate(.workflow = map2(.workflow,
                                best_tune,
                               ~ .x %>%
                                       finalize_workflow(parameters = .y) %>%
                                       fit(impute))
        )

# impute training and validation
train = 
        impute_wflows %>%
        filter(wflow_id == 'game_xgb') %>%
        pluck(".workflow", 1) %>%
        augment(train) %>%
        # impute missigness on averageweight
        mutate(averageweight = case_when(is.na(averageweight) ~ .pred,
                                         TRUE ~ averageweight)) %>%
        select(-.pred)

valid = 
        impute_wflows %>%
        filter(wflow_id == 'game_xgb') %>%
        pluck(".workflow", 1) %>%
        augment(valid) %>%
        # impute missigness on averageweight
        mutate(averageweight = case_when(is.na(averageweight) ~ .pred,
                                         TRUE ~ averageweight)) %>%
        select(-.pred)

# discard what we don't need
rm(impute_last_fits,
   impute_race_res,
   impute_split,
   impute_train_folds
   )

# rm(list=setdiff(ls(), c("categorical_mapping",
#                         "train",
#                         "valid",
#                         "other",
#                         "impute",
#                         "impute_wflows")))


# # model cards
# 
# library(vetiver)
# library(pins)
# 
# model_board <- board_folder(here::here("models", "board"))
# 
# mod = impute_wflows %>% 
#         filter(wflow_id == 'game_norm_lm') %>% pluck(".workflow", 1)
#         
# v <- vetiver_model(mod, "averageweight_lm")
# vetiver_pin_write(model_board, v)
# 
# rmarkdown::draft(
#         "my_model_card.Rmd", 
#         template = "vetiver_model_card", 
#         package = "vetiver"
# )





