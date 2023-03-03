# what: build recipes and model specifications for modeling bgg outcomes

# dependencies:
# preprocessing.R

# basic recipe; little to no preprocessing
# specify 'outcome' variable in recipe
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
                               "log_usersrated",
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
                # remove zero variance
                step_zv(all_predictors())
        
}

# recipe to downsample based on user threshold
downsample_recipe = function(recipe) {
        
        recipe %>%
        step_mutate(threshold = factor(dplyr::case_when(usersrated >=30 ~ 'in', usersrated < 30 ~ 'out'))) %>% 
                themis::step_downsample(threshold,
                                        under_ratio = tune::tune())
        
}

# additional preprocessing
# impute missigness on playing time via median
preproc_recipe_func = function(recipe) {
        
        recipe %>%
                step_impute_median(playingtime,
                                   minplayers,
                                   maxplayers,
                                   minage) %>% # medianimpute numeric predictors
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
                # make time per player variable
                step_mutate(time_per_player = playingtime/ maxplayers) %>% 
                # log time per player and playingtime
                step_log(time_per_player,
                         playingtime,
                         offset = 1) %>%
                # truncate yearpublished
                step_mutate(year = dplyr::case_when(yearpublished < 1900 ~ 1900,
                                                    TRUE ~ yearpublished),
                            role = "predictor")
}

# impute averageweight inside of a recipe
impute_averageweight_recipe_func = function(recipe) {
        
        recipe %>%
                update_role(averageweight,
                            new_role = 'predictor') %>%
                # impute averageweight inside of recipe via a simpler linear model
                step_impute_bag(averageweight,
                                   impute_with = imp_vars(all_predictors(),
                                                          # remove creator variables
                                                          -starts_with("pub_"),
                                                          -starts_with("des_"),
                                                          -starts_with("art_"))
                )
        
}

# add splines
splines_recipe_func = function(recipe) {
        
        recipe %>%
                # truncate yearpublished
                step_mutate(year = dplyr::case_when(yearpublished < 1900 ~ 1900,
                                                   TRUE ~ yearpublished)) %>%
                # # then, add spline for truncated yearpublished
                step_ns(year,
                        deg_free = 5) %>%
                # ### nonlinear effects
                # # spline for number mechanics
                step_ns(number_mechanics,
                        deg_free = 5) %>%
                # # spline for number categories
                step_ns(number_categories,
                        deg_free = 5)
        # # cubic splines for playingtime and time per player
        # step_ns(playingtime,
        #         deg_gree = 3)
        # step_ns(time_per_player,
        #         deg_gree = 3) %>%
        # # check for missingnness
        # check_missing(all_numeric_predictors())
        # 
}

# normalization
norm_recipe_func = function(recipe) {
        
        recipe %>%
                step_normalize(all_numeric_predictors())
        
}

# additional recipes
pca_recipe_func = function(recipe) {
        
        recipe %>%
        step_pca(all_numeric_predictors(),
                 prefix = "PC",
                 threshold = 0.75)
        
}

# workflows and recipes ---------------------------------------------------

# linear regression
lm_spec <-
        linear_reg(mode = "regression") %>%
        set_engine("lm")

# penalized logistic regression
glmnet_spec <- 
        linear_reg(mode = "regression",
                   penalty = tune::tune(),
                   mixture = 0.5) %>%
        set_engine("glmnet")

# specify grid for tuning glmnet
glmnet_grid <- 
        grid_regular(
                penalty(range = c(-5, -.5)),
                levels = 10
        )

# cart
cart_spec <-
        decision_tree() %>%
        set_mode("regression")

# cart grid
cart_grid <- 
        grid_regular(
                cost_complexity(), 
                min_n(), 
                levels = c(cost_complexity = 3, 
                           min_n = 6)
        )

# xgbTree
xgb_spec <-
        boost_tree(
                trees = 500,
                min_n = tune(),
                sample_size = tune(),
                tree_depth = 10,
                stop_iter = 50) %>%
        set_mode("regression") %>%
        set_engine("xgboost",
                   eval_metric = 'rmse')

# # set up grid for tuning
xgb_grid = 
        grid_latin_hypercube(
                tree_depth = tree_depth(range = c(2L, 9L)),
                min_n(),
                sample_size = sample_prop(),
              #  mtry = mtry_prop(c(0.25, 1)),
                learn_rate(),
                size = 30
        )


# metrics and controls ----------------------------------------------------

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

# control for sim_anneal
sim_anneal_ctrl = 
        finetune::control_sim_anneal(
                verbose = T,
                verbose_iter = T,
                save_pred = T,
                save_workflow = T,
                parallel_over = "resamples")


# functions for creating workflows/workflowsets ---------------------------

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

# creates workflow given model and recipe
create_workflow = function(recipe, model) {
        
        workflow() %>%
                add_recipe(recipe) %>%
                add_model(model)
        
}


