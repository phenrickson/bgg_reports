# what: train workflow sets 

# dependencies:
# preprocessing.R
# recipes.R
# models.R

set.seed(1999)

base_rec =
        base_recipe_func("average")

wflow_sets = 
        tibble("outcome" = c("average")) %>%
        nest(-outcome)
        mutate(wflowsets = map(outcome,
                                       ~ workflow_set(
                                               # list of different preprocessors
                                               preproc = list(
                                                       # base recipe
                                                       base = base_rec(.x),
                                                       # recipe with preprocessing/imputation
                                                       preproc = base_rec(.x) %>%
                                                               preproc_recipe_func(),
                                                       # recipe with preprocessing/imputation + pca
                                                       pca = base_rec(.x) %>%
                                                               preproc_recipe_func() %>%
                                                               norm_recipe_func() %>%
                                                               pca_recipe_func(),
                                                       # recipe with splines and normalized features
                                                       norm = base_rec(.x) %>%
                                                               preproc_recipe_func() %>%
                                                               splines_recipe_func() %>%
                                                               norm_recipe_func()),
                                               models = list(
                                                       cart = cart_spec,
                                                       glmnet = glmnet_spec,
                                                       xgb = xgb_spec),
                                               cross = T
                                       ) %>%
                                       # only keep specific combinations of workflows
                                       inner_join(tibble(wflow_id =    
                                                                 c("base_cart",
                                                                   "base_xgb",
                                                                   "preproc_xgb",
                                                                   "preproc_cart",
                                                                   "norm_glmnet",
                                                                   "pca_glmnet")),
                                                  by = c("wflow_id")
                                       ) %>%
                                       # add in custom tuning grids
                                       # cart grid
                                       option_add(param_info = cart_grid, id = c("base_cart",
                                                                                 "preproc_cart")) %>%
                                       # glmnet grid
                                       option_add(param_info = glmnet_grid, id = c("norm_glmnet",
                                                                                   "pca_glmnet")) %>%
                                       # xgb grid
                                       option_add(param_info = glmnet_grid, id = c("base_xgb",
                                                                                   "preproc_xgb"))
        )
        )

        workflow_set(
                # list of different preprocessors
                preproc = list(
                        # base recipe
                        base = base_rec,
                        # recipe with preprocessing/imputation
                        preproc = base_rec %>%
                                preproc_recipe_func(),
                        # recipe with preprocessing/imputation + pca
                        pca = base_rec %>%
                                preproc_recipe_func() %>%
                                norm_recipe_func() %>%
                                pca_recipe_func(),
                        # recipe with splines and normalized features
                        norm = base_rec %>%
                                preproc_recipe_func() %>%
                                splines_recipe_func() %>%
                                norm_recipe_func()),
                models = list(
                        cart = cart_spec,
                        glmnet = glmnet_spec,
                        xgb = xgb_spec),
                cross = T
        ) %>%
        # only keep specific combinations of workflows
        inner_join(tibble(wflow_id =    
                                  c("base_cart",
                                     "base_xgb",
                                     "preproc_xgb",
                                     "preproc_cart",
                                     "norm_glmnet",
                                     "pca_glmnet")),
                   by = c("wflow_id")
        ) %>%
        # add in custom tuning grids
        # cart grid
        option_add(param_info = cart_grid, id = c("base_cart",
                                                  "preproc_cart")) %>%
        # glmnet grid
        option_add(param_info = glmnet_grid, id = c("norm_glmnet",
                                                    "pca_glmnet")) %>%
        # xgb grid
        option_add(param_info = glmnet_grid, id = c("base_xgb",
                                                    "preproc_xgb"))
        
        # create workflow sets
        mutate(wflowsets = map(data,
                               ~ workflow_set(
                                       preproc = list(
                                               # create specific preprocessing for tree based models
                                               tree = base_recipe_func("is_client", .x)  %>%
                                                       # add in factor variables for revenue and employees
                                                       update_role(c("revenueRange",
                                                                     "employeeRange"),
                                                                   new_role = "predictor") %>%
                                                       # trim down to just these variables
                                                       step_rm(all_predictors(),
                                                               -revenue,
                                                               -employeeCount,
                                                               -revenueRange,
                                                               -employeeRange,
                                                               -primaryIndustry,
                                                               -subIndustry) %>%
                                                       step_novel(revenueRange,
                                                                  employeeRange,
                                                                  new_level = "Novel") %>%
                                                       step_unknown(revenueRange,
                                                                    employeeRange,
                                                                    new_level = "Unknown") %>%
                                                       step_dummy(revenueRange,
                                                                  employeeRange,
                                                                  primaryIndustry,
                                                                  subIndustry),
                                               # preprocessing no norm
                                               preproc =  base_recipe_func("is_client", .x) %>%
                                                       preproc_recipe_func,
                                               # simple pre processing keeping only revenue
                                               revenue = base_recipe_func("is_client", .x) %>%
                                                       step_rm(all_predictors(),
                                                               -revenue),
                                               # filter for correlation and normalize
                                               corr = base_recipe_func("is_client", .x) %>%
                                                       preproc_recipe_func %>%
                                                       # correlation filter
                                                       step_corr(threshold = 0.9) %>%
                                                       # normalize predictors
                                                       step_normalize(all_numeric_predictors()),
                                               # normalize
                                               norm = base_recipe_func("is_client", .x) %>%
                                                       preproc_recipe_func %>%
                                                       # normalize predictors
                                                       step_normalize(all_numeric_predictors()),
                                               # spline
                                               spline = base_recipe_func("is_client", .x) %>%
                                                       preproc_recipe_func %>%
                                                       # add splines for employees and revenue
                                                       step_ns(employeeCount,
                                                               revenue,
                                                               deg_free = 3) %>%
                                                       # normalize predictors
                                                       step_normalize(all_numeric_predictors()),
                                               # categorical for revenue and employeeCount
                                               categorical = base_recipe_func("is_client", .x) %>%
                                                       preproc_recipe_func %>%
                                                       # remove revenue and revenue range
                                                       update_role(c("revenue",
                                                                     "employeeCount"),
                                                                   new_role = "id") %>%
                                                       # add revenue range and employee as predictors
                                                       update_role(c("revenueRange",
                                                                     "employeeRange"),
                                                                   new_role = "predictor") %>%
                                                       # now novel and dummy these
                                                       step_novel(revenueRange,
                                                                  employeeRange,
                                                                  new_level = "Novel") %>%
                                                       step_unknown(revenueRange,
                                                                    employeeRange,
                                                                    new_level = "Unknown") %>%
                                                       step_dummy(revenueRange,
                                                                  employeeRange) %>%
                                                       step_normalize(all_numeric_predictors()),
                                               # pca
                                               pca = base_recipe_func("is_client", .x) %>%
                                                       preproc_recipe_func %>%
                                                       # normalize predictors
                                                       step_normalize(all_numeric_predictors()) %>%
                                                       # apply pca
                                                       step_pca(all_numeric_predictors())
                                       ),
                                       models = list(cart = cart_class_mod,
                                                     glmnet = glmnet_class_mod,
                                                     glm = glm_class_mod,
                                                     xgb = xgb_class_mod,
                                                     mars = mars_class_mod),
                                       cross = T) %>%
                                       # keep workflows we want
                                       inner_join(tibble(wflow_id =    
                                                                 c(
                                                                         "tree_cart",
                                                                         "tree_xgb",
                                                                         "preproc_cart",
                                                                         "preproc_xgb",
                                                                         "revenue_glm",
                                                                         "corr_glm",
                                                                         "norm_glmnet",
                                                                         "norm_mars",
                                                                         "spline_glmnet",
                                                                         "categorical_glmnet",
                                                                         "pca_glmnet"
                                                                 )),
                                                  by = c("wflow_id")) %>%
                                       # add in custom tuning grids
                                       option_add(param_info = cart_grid, id = "tree_cart") %>%
                                       option_add(param_info = cart_grid, id = "preproc_cart") %>%
                                       option_add(param_info = glmnet_grid, id = "norm_glmnet") %>%
                                       option_add(param_info = glmnet_grid, id = "pca_glmnet") %>%
                                       option_add(param_info = glmnet_grid, id = "spline_glmnet") %>%
                                       option_add(param_info = glmnet_grid, id = "categorical_glmnet") %>%
                                       option_add(param_info = mars_grid, id = "norm_mars") %>%
                                       option_add(grid = xgb_grid, id = "preproc_xgb") %>%
                                       option_add(grid = xgb_grid, id = "tree_xgb")
        )
        )