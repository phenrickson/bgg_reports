# what: train workflow sets to impute averageweight

# dependencies:
# preprocessing.R
# recipes_and_models.R


# filters to only games with at least 25 user ratings and do not have missingness on average weight
averageweight_train_and_resamples =  
        create_train_and_resamples(train %>%
                                           filter(!is.na(averageweight)) %>%
                                           filter(usersrated >= 25),
                                   'averageweight')
# creates recipe 
# remove creator features from this recipe
base_rec =
        base_recipe_func(data = averageweight_train_and_resamples$training %>%
                                 select(-starts_with("pub_"),
                                        -starts_with("des_"),
                                        -starts_with("art_")),
                         outcome = 'averageweight')

# creates preprocessors
preprocessors =
        list(
                # using all features
                preproc = base_rec %>%
                        preproc_recipe_func(),
                norm = base_rec %>%
                        preproc_recipe_func() %>%
                        splines_recipe_func() %>%
                        norm_recipe_func(),
                corr = base_rec %>%
                        preproc_recipe_func() %>%
                        splines_recipe_func() %>%
                        norm_recipe_func() %>%
                        step_corr(all_numeric_predictors(),
                                  threshold = .9)
        )

# models
models =
        list(
                lm = lm_spec,
                glmnet = glmnet_spec,
                xgb = xgb_spec
        )

# create workflow set
averageweight_wflows =
        workflow_set(
                # preprocessors
                preproc = preprocessors,
                # models
                models = models,
                cross = T
        ) %>%
        # keep selected
        filter(wflow_id %in% c(
                "preproc_xgb"
                #,"preproc_cart"
                ,"corr_lm"
                ,"norm_glmnet")
        ) %>%
        # add tuning options
     #   option_add(grid = cart_grid, id = c("preproc_cart")) %>%
        # glmnet grid
        option_add(grid = glmnet_grid, id = c("norm_glmnet")) %>%
        # xgb grid
        option_add(grid = xgb_grid, id = c("preproc_xgb"))


# set parallel
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)

# now tune
averageweight_wflows = 
        averageweight_wflows %>%
        workflow_map(
                fn = 'tune_race_anova',
                seed = 1999,
                metrics = reg_metrics,
                resamples = averageweight_train_and_resamples$resamples,
                control = race_ctrl)
        
# collect results
averageweight_results = 
        averageweight_wflows %>%
        rank_results(select_best = T, rank_metric = 'rmse')

# collect predictions
averageweight_wflows %>%
        collect_predictions(select_best = T, metric = 'rmse') %>%
        ggplot(aes(x=.pred,
                   y= averageweight))+
        geom_point(size = 0.5)+
        facet_wrap(~wflow_id ~.)+
        ggpubr::stat_cor(p.accuracy = 0.01,
                         col = 'blue')+
        theme_bw()

# best model
averageweight_best_mod = 
        averageweight_results %>%
        filter(.metric == 'rmse') %>%
        slice_min(mean, n=1, with_ties = F) %>%
        pull(wflow_id)

# best tune for best model
averageweight_best_tune = 
        averageweight_wflows %>%
        mutate(best_tune = map(result,
                               ~ .x %>% select_best(metric = 'rmse', n =1))) %>%
        filter(wflow_id == averageweight_best_mod) %>%
        select(wflow_id, best_tune) %>%
        unnest(best_tune)
 
# finalize workflow
averageweight_wflow = 
        averageweight_wflows %>%
        extract_workflow(id = averageweight_best_mod) %>%
        finalize_workflow(parameters = averageweight_best_tune)

# fit
averageweight_model = 
        averageweight_wflow %>%
        fit(averageweight_train_and_resamples$training)

# create function to impute
impute_averageweight = function(data) {
        
        averageweight_model %>%
                # predict
                augment(data) %>%
                # replace
                mutate(averageweight = case_when(is.na(averageweight) ~ .pred,
                                                 TRUE ~ averageweight)) %>%
                # truncate
                mutate(averageweight = case_when(averageweight > 5 ~ 5,
                                                 averageweight < 1 ~ 1,
                                                 TRUE ~ averageweight)) %>%
                # remove
                select(-.pred)
        
}

# get model board
model_board = board_folder(here::here("models", "board"))

# pin
pin_write(model_board,
          averageweight_model,
          name = "averageweight_model",
          description = paste("model trained to predict averageweight on games published through", end_train_year),
          versioned=T)
          
          
rm(averageweight_wflows,
   averageweight_wflow,
   averageweight_results,
   averageweight_train_and_resamples,
   base_rec,
   preprocessors)

        
