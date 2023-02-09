# what: create workflows for model development

# dependencies:
# preprocessing.R
# recipes.R


# make validation/test splits ------------------------------------------------------------------

# make an initial split based on previously defined splits
valid_split = make_splits(list(analysis = seq(nrow(train)),
                               assessment = nrow(train) + seq(nrow(valid))),
                          data  = bind_rows(train, valid))

# workflows and recipes ---------------------------------------------------


# linear regression
lm_spec <-
        linear_reg(mode = "regression")


# penalized logistic regression
glmnet_spec <- 
        linear_reg(mode = "regression",
                     penalty = tune::tune(),
                     mixture = 0.5) %>%
        set_engine("glmnet")

# specify grid for tuning glmnet
glmnet_grid <- 
        glmnet_spec %>%
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
              #  sample_size = tune(),
                min_n = tune(),
                mtry = tune(), # randomness
                learn_rate = tune(),  
                tree_depth = tune(),
                stop_iter = 50) %>%
        set_mode("regression") %>%
        set_engine("xgboost",
                   eval_metric = 'rmse',
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


# # specify regression metrics
reg_metrics<-metric_set(yardstick::rmse,
                        yardstick::mae,
                        yardstick::mape,
                        yardstick::rsq)

# control for resamples
control_grid <- control_resamples(save_pred = TRUE, 
                                  save_workflow = TRUE,
                                  allow_par = T,
                                  verbose = T,
                                  parallel_over = "resamples")

