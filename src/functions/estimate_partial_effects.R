# what: run lasso on game designers + complexity + year effects to find designers associated with outcomes


# requirements-------------------------------------------------------------------

# requires games on which to train models:
# games_train


# then, makes use of categorical datasets:
# game_publishers
# game_artists

# # script that loads functions and local datasets
# source(here::here("src", "helpers", "exploratory_setup.R"))
# 
# # functions used in handling categorical features
# source(here::here("src", "functions", "preprocessing_functions.R"))
# 
# functions --------------------------------------------------------------------

# for a categorical games dataset (game_designers, game_publishers, ...):
# estimate the partial effect of each variable on a bgg outcome

# outcome can be one of ('bayesaverage', 'average', 'averageweight', or 'usersrated')
# train year is the last year used in training
# min games is the minimum number of games associated with a category

# categorical = g
estimate_partial_effects = function(train_games,
                                    games_categorical,
                                    outcome = 'bayesaverage',
                                    min_games = 5) {

        message(paste(nrow(train_games), "games in dataset through", max(train_games$yearpublished, na.rm=T)))
        
        # get designer dummies
        # filter to only games in tidy games
        # set minimum number of games
        
        message("preparing data...")
        games_categorical = 
                games_categorical %>%
                filter(game_id %in% train_games$game_id) %>%
                group_by(id, value) %>%
                mutate(num_games = n_distinct(game_id)) %>%
                ungroup() %>%
                filter(num_games > min_games)
        
        # mapping
        games_categorical_mapping =
                games_categorical %>%
                distinct(type, id, value) %>%
                mutate(tidied = abbreviate_categorical(value)) %>%
                select(type, id, value, tidied)
        
        # pivot
        games_categorical_pivoted = 
                games_categorical %>%
                pivot_categorical_variables()
        
        # now join this with outcome
        data = 
                train_games %>%
                select(game_id, name, yearpublished, averageweight, average, bayesaverage, usersrated) %>%
                left_join(.,
                          games_categorical_pivoted,
                          by = c("game_id")
                ) %>%
                # replace missingness in designer with zero
                mutate_if(is.numeric, replace_na, 0)
        
        # modeling ----------------------------------------------------------------
        
        # model
        # penalized regression
        glmnet_reg_mod = parsnip::linear_reg(mixture = 1, 
                                             penalty = tune::tune(),
                                             engine = 'glmnet')
        
        # specify grid for tuning
        glmnet_grid <- tibble(penalty = 10^seq(-3, -1, 
                                               length.out = 20))
        
        # folds for tuning
        set.seed(1999)
        folds = vfold_cv(data,
                         strata = usersrated,
                         v = 5)
        
        # specify regression metrics
        reg_metrics<-metric_set(yardstick::rmse)
        
        # control for resamples
        keep_pred <- control_resamples(save_pred = TRUE, 
                                       save_workflow = TRUE)
        # recipe
        rec = recipe(x = data) %>%
                update_role(game_id,
                            name,
                            average,
                            bayesaverage,
                            usersrated,
                            yearpublished,
                            new_role = "id") %>%
                # make others predictors
                update_role(all_numeric(),
                            -has_role("id"),
                            new_role = "predictor") %>%
                # address missingness
                step_impute_median(averageweight) %>%
                # compress yearpublished
                # denote games published before 1900 with indiciator
                step_mutate(published_prior_1900 = dplyr::case_when(yearpublished < 1900 ~ 1,
                                                                    TRUE ~ 0)) %>%
                # then truncate to post 1900
                step_mutate(year = case_when(yearpublished < 1900 ~ 1900,
                                             TRUE ~ yearpublished)) %>%
                # then, add spline for truncated yearpublished
                step_ns(year,
                        deg_free = 9) %>%
                # then, convert averageweight to 0 1
                step_range(averageweight, min = 0, max =1) %>%
                # remove zero variance predictors
                step_zv(all_predictors())
        
        message("fitting models...")
        
        # function to make workflows given outcome
        make_workflow = function(outcome) {
                
                if(length(outcome) != 1) {
                        error("outcome can only have length = 1")
                }
                
                wflow = workflow() %>%
                        add_recipe(rec %>%
                                           update_role(all_of(outcome), 
                                                       new_role = "outcome")) %>%
                        add_model(glmnet_reg_mod)
        }
        
        
        # fit workflows and get objects
        wflow_fits = 
                # create data frame with outcome vector
                tibble(outcome = c(outcome)) %>%
                # create workflows
                mutate(wflow = map(outcome, 
                                   ~  make_workflow(.))) %>%
                # tune
                mutate(tune_rs = 
                               map(wflow,
                                   ~ .x %>%
                                           tune_grid(.,
                                                     metrics = reg_metrics,
                                                     resamples = folds))) %>%
                # select best tune
                mutate(best_tune = map(tune_rs,
                                       ~ select_by_one_std_err(.x,
                                                               penalty,
                                                               metric = 'rmse'))) %>%
                # fit on data
                mutate(fitted= map2(wflow,
                                    best_tune,
                                    ~ .x %>%
                                            finalize_workflow(parameters = .y) %>%
                                            fit(data))) %>%
                # extract fit
                mutate(fit = map(fitted,
                                 ~ extract_fit_parsnip(.x))) %>%
                # get augmented on training set
                mutate(augmented = map(fitted,
                                       ~ augment(., new_data = data))) %>%
                # get coefs
                mutate(coefs = map(fit,
                                   ~ tidy(.x)))
        
        # get coefs
        coefs = wflow_fits %>%
                select(outcome, coefs) %>%
                unnest(coefs) %>%
                filter(!grepl("yearpublished|averageweight|Intercept", term)) %>%
                # exponentiate users rated (logged outcome)
                mutate(estimate = case_when(outcome == 'usersrated' ~ (exp(estimate)-1),
                                            TRUE ~ estimate)) %>%
                # round
                mutate(estimate = round(estimate, 3)) %>%
                # remove zeroes
                filter(estimate != 0)
        
        # select games_categorical above a minimum
        partial_effects = 
                coefs %>% 
                # impose an (admittedly arbitrary) minimum
                filter(abs(estimate) > 0.25) %>%
                mutate(term = str_sub(term, start = 5L, end = -1L)) %>%
                # exponentitate if using (logged) usersrated
                mutate(estimate = case_when(outcome == 'usersrated' ~ (exp(estimate)-1),
                                            TRUE ~ estimate)) %>%
                left_join(.,
                          games_categorical_mapping %>%
                                  select(type, tidied, value, id),
                          by = c('term' = 'tidied')) %>%
                select(outcome, type, id, value, term, estimate)
        
        out = list("partial_effects" = partial_effects,
                   "workflows" = wflow_fits)
        
        return(out)
        
}

# # not run
# returned = estimate_partial_effects(
#         train_games,
#         game_designers,
#         train_year = 2021,
#         min_ratings = 50,
#         min_games = 5)
