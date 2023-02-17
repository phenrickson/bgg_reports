# what: train workflow sets

# dependencies:
# preprocessing.R
# recipes_and_models.R


# missingness -------------------------------------------------------------

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


# setup for training ------------------------------------------------------------------

# # specify regression metrics
reg_metrics<-metric_set(yardstick::rmse,
                        yardstick::mae,
                        yardstick::mape,
                        yardstick::rsq)

# control for resamples
ctrl <- control_resamples(save_pred = TRUE, 
                                  save_workflow = F,
                                  allow_par = T,
                                  verbose = T,
                                  parallel_over = "resamples")

# control for racing
race_ctrl = 
        finetune::control_race(
                save_pred = TRUE,
                parallel_over = "resamples",
                verbose = TRUE,
                verbose_elim = TRUE,
                save_workflow = F
        )

sim_anneal_ctrl = 
        finetune::control_sim_anneal(
                verbose = T,
                verbose_iter = T,
                save_pred = T,
                save_workflow = F,
                parallel_over = "resamples")

# workflow sets -----------------------------------------------------------


# list of model specs to consider
models =   list(
        cart = cart_spec,
        lm = lm_spec,
        glmnet = glmnet_spec,
        xgb = xgb_spec)

# creates custom training set and resamples given data and outcome
create_train_and_resamples = function(data, outcome) {
        
        if (colSums(is.na(data[,paste(outcome)])) > 0) {
                
                stop(paste("missingness in", outcome))
                
        }

        message(paste("creating resamples for", outcome, "with", nrow(data), "obs"))

        set.seed(1999)
        resamples = vfold_cv(data,
                             v = 5,
                             strata = !!rlang::sym(outcome))
        
        return(list("training" = data,
                    "resamples" = resamples))
                
}

# given specified recipes, build preprocessors by outcome
create_preprocessors = function(data, outcome) {
        
        
        if (outcome != 'averageweight') {
                
                list(
                        # basic preprocessing recipe without creator features
                        preproc =
                                base_recipe_func(data = data,
                                                 outcome = outcome) %>%
                                # preproc and imputation
                                preproc_recipe_func(),
                        # add in splines and norm
                        norm =
                                base_recipe_func(data = data,
                                                 outcome = outcome) %>%
                                # preproc and imputation
                                preproc_recipe_func() %>%
                                # impute averageweight
                                impute_averageweight_recipe_func() %>%
                                # add splines
                                splines_recipe_func() %>%
                                # normalize
                                norm_recipe_func(),
                        # add in splines, normalize, then apply pca
                        pca =
                                base_recipe_func(data = data,
                                                 outcome = outcome) %>%
                                # preproc and imputation
                                preproc_recipe_func() %>%
                                # impute averageweight
                                impute_averageweight_recipe_func() %>%
                                # add splines
                                splines_recipe_func() %>%
                                # normalize
                                norm_recipe_func() %>%
                                # pca
                                pca_recipe_func())
        }
        
        else if (outcome == 'averageweight')
                
        { list(
                        # basic preprocessing recipe without creator features
                        preproc =
                                base_recipe_func(data = data,
                                                 outcome = outcome) %>%
                                # preproc and imputation
                                preproc_recipe_func(),
                        # add in splines and norm
                        norm =
                                base_recipe_func(data = data,
                                                 outcome = outcome) %>%
                                # preproc and imputation
                                preproc_recipe_func() %>%
                                # add splines
                                splines_recipe_func() %>%
                                # normalize
                                norm_recipe_func(),
                        # add in splines, normalize, then apply pca
                        pca =
                                base_recipe_func(data = data,
                                                 outcome = outcome) %>%
                                # preproc and imputation
                                preproc_recipe_func() %>%
                                # add splines
                                splines_recipe_func() %>%
                                # normalize
                                norm_recipe_func() %>%
                                # pca
                                pca_recipe_func())

        }
        
}

# function to create workflow sets given a dataset
build_workflow_sets = function(data, outcome, models) {
        
        # create train and resamples based on input data
        setup = create_train_and_resamples(data, outcome)
        
        # extract quantities needed for workflow sets
        training = setup$training
        resamples = setup$resamples
        
        message(paste("creating workflow sets for", outcome, "with",  nrow(training), "obs"))
        
        # create preprocessors given data and outcome
        preprocessors = create_preprocessors(training, outcome)
        
        # create workflow sets
        workflow_sets = 
                workflow_set(
                        preproc = preprocessors,
                        models = models,
                        cross = T
                )
        
        return(list(
                "training" = training,
                "resamples" = resamples,
                "workflow_sets" = workflow_sets))
        
}

# function to tune over workflow sets 
tune_workflow_sets = function(build_workflow_sets,
                              method = "tune_race_anova",
                              list = build_workflow_sets$workflow_sets$wflow_id) {
        
        if (method == 'tune_race_anova') {
                
                message("tuning workflow sets via tune_race_anova")
                
                # run tuning
                tune_res = 
                        build_workflow_sets$workflow_sets %>%
                        # add specific options for xgb and glmnet
                        option_add(grid = cart_grid, id = c("preproc_cart")) %>%
                        # glmnet grid
                        option_add(grid = glmnet_grid, id = c("norm_glmnet", "pca_glmnet")) %>%
                        # xgb grid
                        option_add(grid = xgb_grid, id = c("preproc_xgb")) %>%
                        # keep only wflow ids in specified list
                        filter(wflow_id %in% list) %>%
                        workflow_map(
                                fn = method,
                                seed = 1999,
                                resamples = build_workflow_sets$resamples,
                                control = race_ctrl)
        }
        
        else if (method == 'tune_sim_anneal') {
                
                message("tuning workflow sets via tune_sim_anneal")
                
                tune_res = 
                        build_workflow_sets$workflow_sets %>%
                        # keep only wflow ids in specified list
                        filter(wflow_id %in% list) %>%
                        # remove grids
                        option_add_parameters() %>%
                        workflow_map(
                                fn = method,
                                seed = 1999,
                                resamples = build_workflow_sets$resamples,
                                control = sim_anneal_ctrl)
                
        } 
        
        return(list("tune_res" = tune_res,
                    "resamples" = build_workflow_sets$resamples,
                    "training" = build_workflow_sets$training))
        
}


# tuning ------------------------------------------------------------------


# set parallel
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)


# tune workflows for averageweight
# do not use pub, des, art features
# filtering to games that have at least 25 user ratings
averageweight_workflows =
        # create workflow sets
        build_workflow_sets(data = train %>%
                                    select(-starts_with("pub_"),
                                           -starts_with("des_"),
                                           -starts_with("art_")) %>%
                                    filter(!is.na(averageweight)) %>%
                                    filter(usersrated >= log1p(25)), 
                            outcome = 'averageweight', 
                            models = models) %>%
        # tune workflow sets
        tune_workflow_sets(.,
                           method = "tune_race_anova",
                           list = c("norm_glmnet",
                                    "pca_glmnet",
                                    "norm_lm",
                                    "preproc_cart",
                                    "preproc_xgb"))

# tune workflow sets for average
# filtering to games that have at least 25 user ratings
average_workflows =
        # create workflow sets
        build_workflow_sets(data = train %>%
                                             filter(usersrated >= log1p(25)), 
                                     outcome = 'average', 
                                     models = models) %>%
        # tune workflow sets
        tune_workflow_sets(.,
                           method = "tune_race_anova",
                           list = c("norm_glmnet",
                                    "pca_glmnet",
                                    "norm_lm",
                                    "preproc_cart",
                                    "preproc_xgb"))

# tune workflow sets for usersrated
usersrated_workflows =
        # create workflow sets
        build_workflow_sets(data = train,
                            outcome = 'usersrated', 
                            models = models) %>%
        # tune workflow sets
        tune_workflow_sets(.,
                           method = "tune_race_anova",
                           list = c("norm_glmnet",
                                    "pca_glmnet",
                                    "norm_lm",
                                    "preproc_cart",
                                    "preproc_xgb"))

# results -----------------------------------------------------------------

rs_board = board_folder(here::here("models", "rs"))

# save
pins::pin_write(rs_board,
                average_workflows$tune_res,
                name = "average_workflows",
                description = paste("tuning results for average workflows trained through", end_train_year))

# save
pins::pin_write(board = rs_board,
                averageweight_workflows$tune_res,
                name = "averageweight_workflows",
                description = paste("tuning results for averageweight workflows trained through", end_train_year))

# save
pins::pin_write(rs_board,
                usersrated_workflows$tune_res,
                name = "usersrated_workflows",
                description = paste("tuning results for usersrated workflows trained through", end_train_year))


