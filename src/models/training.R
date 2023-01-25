# what: trains models on datasets assembled from preprocessing.R

# dependencies:
# requires objects from preprocessing.R


# splits ------------------------------------------------------------------


# make an initial split based on previously defined splits
valid_split = make_splits(list(analysis = seq(nrow(train)),
                               assessment = nrow(train) + seq(nrow(valid))),
                          data  = bind_rows(train, valid))

# workflows and recipes ---------------------------------------------------

# penalized logistic regression
glmnet_spec <- 
        linear_reg(mode = "regression",
                     penalty = tune::tune(),
                     mixture = 0.5) %>%
        set_engine("glmnet")

# specify grid for tuning glmnet
glmnet_grid <- 
        glmnet_reg_mod %>%
        extract_parameter_set_dials() %>%
        update(penalty = penalty(range = c(-5, -0.5)))

# cart
cart_spec <-
        decision_tree() %>%
        set_mode("regression")

# cart grid
cart_grid <- 
        grid_regular(
                cost_complexity(), 
                min_n(), 
                levels = c(3, 6)
        )

# xgbTree
xgb_spec <-
        boost_tree(
                trees = 250,
                sample_size = tune(),
                min_n = tune(),
                mtry = tune(),         ## randomness
                learn_rate = tune(),  
                tree_depth = tune(),
                stop_iter = 50) %>%
        set_mode("regression") %>%
        set_engine("xgboost",
                   eval_metric = 'rmse',
                   # for mtry to be in [0,1]
                   counts = F)

# # set up grid for tuning
xgb_grid = 
        grid_latin_hypercube(
                tree_depth(),
                min_n(),
                sample_size = sample_prop(),
                mtry = mtry_prop(c(0.1, 1)),
                learn_rate(),
                size = 30
        )


# # specify regression metrics
reg_metrics<-metric_set(yardstick::rmse,
                        yardstick::mae,
                        yardstick::mape,
                        yardstick::rsq)

# control for resamples
control_grid <- control_resamples(save_pred = TRUE, 
                                  save_workflow = TRUE,
                                  allow_par = T,
                                  parallel_over = "resamples")


# recipes -----------------------------------------------------------------


# basic recipe; little to no preprocesing
base_recipe_func = function(outcome) {
        
        rec = recipe(x=train) %>%
                # set id variables
                update_role(game_id,
                            name,
                            yearpublished,
                            image,
                            thumbnail,
                            description,
                            load_ts,
                            new_role = "id") %>%
                # set outcome varable
                update_role(all_of(outcome),
                            new_role = "outcome") %>%
                # set all others as predictors
                update_role(-has_role("id"),
                            -has_role("outcome"),
                            new_role = "predictor") %>%
                # missingness
                step_mutate(missing_image = case_when(is.na(im))
                # number of mechanics
                step_mutate(number_mechanics = rowSums(across(starts_with("mec_")))) %>%
                # mumber of categories
                step_mutate(number_categories = rowSums(across(starts_with("cat_")))) %>%
                # remove zero variance
                step_zv(all_predictors())
                # feature engineering
                # year effects
        
}
