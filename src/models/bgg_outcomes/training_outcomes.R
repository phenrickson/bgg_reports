# what: train workflow sets for bgg outcomes

# dependencies:
# preprocessing.R
# recipes_and_models.R
# 



# impute averageweight ----------------------------------------------------

# impute averageweight in training set
train_imputed = impute_averageweight(train %>%
                                     filter(!is.na(bayesaverage)))

# impute averageweight in valid set
valid_imputed = impute_averageweight(valid)


# setup for training ------------------------------------------------------------------


# # specify regression metrics
reg_metrics<-metric_set(yardstick::rmse,
                        yardstick::mae,
                        yardstick::mape,
                        yardstick::rsq)

# control for resamples
ctrl <- control_resamples(save_pred = TRUE, 
                                  save_workflow = T,
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
                save_workflow = T
        )

# workflow sets -----------------------------------------------------------


# list of model specs to consider
models =   list(
        cart = cart_spec,
        lm = lm_spec,
        glmnet = glmnet_spec,
        xgb = xgb_spec)
        
# given specified recipes, build preprocessors by outcome
create_preprocessors = function(data, outcome) {
        
        # preproc and impute averageweight (as a fallback)
        preproc_recipe = 
                base_recipe_func(data = data,
                                 outcome = outcome) %>%
                preproc_recipe_func() %>%
                impute_averageweight_recipe_func()
        
        # preproc and impute averageweight (if needed)
        norm_recipe = 
                preproc_recipe %>%
                splines_recipe_func() %>%
                norm_recipe_func()
        
        # correlation filter for linear model
        corr_recipe = 
                norm_recipe %>%
                step_corr(all_numeric_predictors(),
                          threshold = .9)
        
        # add correlation filter
        preprocessors = list(
                preproc = preproc_recipe,
                norm = norm_recipe,
                corr = corr_recipe)
        
        return(preprocessors)
        
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
tune_workflow_sets = function(workflow_sets,
                              resamples,
                              method = 'tune_race_anova') {
        
        n_sets = nrow(workflow_sets)

        message(paste("tuning", n_sets, "workflows via tune_race_anova"))
        
        # run tuning
        tune_res = 
                workflow_sets %>%
                # keep only wflow ids in specified list
                workflow_map(
                        fn = method,
                        seed = 1999,
                        resamples = resamples,
                        control = race_ctrl)
        
        return(tune_res)
}

# implement previous functions in one go
build_tune_and_collect_workflows = function(data, outcome, models) {
        
        # build
        workflow_objs = 
                build_workflow_sets(
                        data = data,
                        outcome = outcome,
                        models = models
                )
        
        # get workflow sets
        workflow_sets = 
                workflow_objs$workflow_sets %>%
                filter(wflow_id %in% c('preproc_xgb',
                                       'preproc_cart',
                                       'corr_lm',
                                       'norm_glmnet')) %>%
                # add tuning options
                option_add(grid = cart_grid, id = c("preproc_cart")) %>%
                # glmnet grid
                option_add(grid = glmnet_grid, id = c("norm_glmnet")) %>%
                # xgb grid
                option_add(grid = xgb_grid, id = c("preproc_xgb"))
        
        # tune
        workflows_res = 
                tune_workflow_sets(
                        workflow_sets = workflow_sets,
                        method = 'tune_race_anova',
                        resamples = workflow_objs$resamples)
        
        # collect 
        message("collecting metrics")
        
        # metrics
        metrics = workflows_res %>%
                rank_results(select_best = T)
        
        # predictions
        message("collecting predictions")
        
        predictions = workflows_res %>% 
                collect_predictions(select_best = T, metric = 'rmse') %>%
                # join with games used in training
                left_join(.,
                          workflow_objs$training %>%
                                  mutate(.row = row_number()) %>%
                                  select(.row, game_id, name, yearpublished),
                          by = c(".row"))
        
        # best tune
        message('selecting best tune')
        workflows_res = workflows_res %>%
                mutate(best_tune = map(result, select_best, n = 1, metric = 'rmse'))
        
        return(list(
                "workflows_res" = workflows_res,
                "metrics" = metrics,
                "predictions" = predictions,
                "training" = workflow_objs$training,
                "resamples" = workflow_objs$resamples)
        )
        
}

# select from workflow sets, finalize, and fit
select_and_finalize_workflow = function(workflow_obs) {
        
        # get best mod
        best_mod = 
                average_workflow_objs$metrics %>% 
                filter(rank ==1 & .metric == 'rmse') %>% 
                pull(wflow_id)
        
        # get best tune
        best_tune =
                average_workflow_objs$workflows_res %>%
                filter(wflow_id == best_mod) %>%
                select(wflow_id, best_tune) %>%
                unnest(best_tune)
        
        # finalize
        wflow = 
                average_workflow_objs$workflows_res %>%
                extract_workflow(id = best_mod) %>%
                finalize_workflow(parameters = best_tune)
        
        return(wflow)
        
}


# tuning ------------------------------------------------------------------


# create and tune workflows for average
# train and evaluate on games with at least 25 user ratings
average_workflow_objs = 
        build_tune_and_collect_workflows(
                data = impute_averageweight(train) %>% 
                        filter(usersrated > 25),
                outcome = 'average',
                models = models)

# create and tune workflows for usersrated
# train and evaluate on games with at least 25 user ratings
usersrated_workflow_objs = 
        build_tune_and_collect_workflows(
                data = impute_averageweight(train) %>% 
                        filter(usersrated > 25),
                outcome = 'log_usersrated',
                models = models)

# create and tune workflows for usersrated
bayesaverage_workflow_objs = 
        build_tune_and_collect_workflows(
                data = impute_averageweight(train) %>% 
                        filter(!is.na(bayesaverage)),
                outcome = 'bayesaverage',
                models = models)


# finalize workflows --------------------------------------------------------

### average
# finalize workflow
average_wflow =
        select_and_finalize_workflow(average_workflow_objs)

# fit
average_fit =
        average_wflow %>%
        fit(average_workflow_objs$training)

### usersrated
# finalize workflow
usersrated_wflow =
        select_and_finalize_workflow(usersrated_workflow_objs)

# fit
usersrated_fit =
        usersrated_wflow %>%
        fit(usersrated_workflow_objs$training)


### bayesaverage
bayesaverage_wflow =
        select_and_finalize_workflow(bayesaverage_workflow_objs)

# fit
bayesaverage_fit =
        bayesaverage_wflow %>%
        fit(bayesaverage_workflow_objs$training)


# average_results %$% 
#         predictions %>%
#         ggplot(aes(x=.pred,
#                    y=average))+
#         geom_point(alpha = 0.5)+
#         facet_wrap(~wflow_id,
#                    ncol = 3)+
#         coord_obs_pred()+
#         theme_bw()+
#         ggpubr::stat_cor()
# 
# averageweight_results %$% 
#         predictions %>%
#         ggplot(aes(x=.pred,
#                    y=averageweight))+
#         geom_point(alpha = 0.5)+
#         facet_wrap(~wflow_id,
#                    ncol = 3)+
#         coord_obs_pred()+
#         theme_bw()+
#         ggpubr::stat_cor()
# 
# usersrated_results %$% 
#         predictions %>%
#         ggplot(aes(x=.pred,
#                    y=usersrated))+
#         geom_point(alpha = 0.5)+
#         facet_wrap(~wflow_id,
#                    ncol = 3)+
#         coord_obs_pred()+
#         theme_bw()+
#         ggpubr::stat_cor()

