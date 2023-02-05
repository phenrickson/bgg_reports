# what: build recipes for modeling games data

# dependencies:
# requires train from preprocessing 

# basic recipe; little to no preprocesing
base_recipe_func = function(outcome) {
        
        recipe(x=train) %>%
                # set id variables
                update_role(game_id,
                            name,
                            yearpublished,
                            image,
                            thumbnail,
                            description,
                            load_ts,
                            average,
                            averageweight,
                            bayesaverage,
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
                step_mutate(number_mechanics = rowSums(across(starts_with("mec_")))) %>%
                # mumber of categories
                step_mutate(number_categories = rowSums(across(starts_with("cat_")))) %>%
                # big box/deluxe/anniversary edition
                step_mutate(deluxe_edition = case_when(grepl("kickstarter|big box|deluxe|mega box",
                                                             description) ~ 1,
                                                       TRUE ~ 0)) %>%
                # word count
                step_mutate(word_count = stringi::stri_count_words(description)) %>%
                step_mutate(word_count = replace_na(word_count, 0)) %>%
                # remove zero variance
                step_zv(all_predictors())
        
}

# some preprocessing/imputation
preproc_recipe_func = function(recipe) {
        
        recipe %>%
                # missingness
                step_mutate(missing_minage = case_when(is.na(minage) ~ 1,
                                                       TRUE ~ 0)) %>%
                step_mutate(missing_playtingtime = case_when(is.na(playingtime) ~ 1,
                                                             TRUE ~ 0)) %>%
                # impute missigness on playing time via median
                step_impute_median(playingtime,
                                   minplayers,
                                   maxplayers,
                                   minage) %>% # medianimpute numeric predictors
                # truncate player counts
                step_range(minplayers, 
                           min = 0, max = 10) %>%
                step_range(maxplayers,
                           min = 1, max= 20) %>%
                # remove
                step_rm(minplaytime, maxplaytime) %>%
                # make time per player variable
                step_mutate(time_per_player = playingtime/ maxplayers) %>% 
                # log time per player and playingtime
                step_log(time_per_player,
                         playingtime,
                         offset = 1) %>%
                # apply near zero variance filter to selected
                step_nzv(all_predictors(),
                         -starts_with("pub_"),
                         -starts_with("art_"),
                         -starts_with("des_"),
                         freq_cut = 100/1) %>%
                ### year effects
                # make dummy for pre 1900
                step_mutate(published_prior_1900 = dplyr::case_when(yearpublished < 1900 ~ 1,
                                                                    TRUE ~ 0)) %>%
                # then truncate to post 1900
                step_mutate(year = case_when(yearpublished < 1900 ~ 1900,
                                             TRUE ~ yearpublished))
        
}

# add splines
splines_recipe_func = function(recipe) {
        
        recipe %>%
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
        
        
# not run
pca = base_recipe_func("average") %>%
        preproc_recipe_func() %>%
        splines_recipe_func() %>%
        # add specific update for averageweight
        update_role("averageweight",
                    new_role = "predictor") %>%
        step_impute_median(averageweight) %>%
        # normalize
        norm_recipe_func() %>%
        # pca
        pca_recipe_func() %>%
        # prep on train
        prep(train)
