# what: functions used for creating/preprocessing categorical features



# preprocessing functions -------------------------------------------------


# tidy games 
tidy_games = function(games) {
        
        # apply consistent prep to full table
        prepped =
                games %>%
                # change zeros to missingness
                mutate_at(
                        vars(c("yearpublished",
                               "averageweight",
                               "average",
                               "bayesaverage",
                               "stddev",
                               "minplayers",
                               "maxplayers",
                               "minage",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime")),
                        ~ na_if(., 0)) %>%
                # change some integers to numeric
                mutate_at(
                        vars(c("minage",
                               "minplayers",
                               "maxplayers",
                               "usersrated",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime")),
                        as.numeric
                ) 
        
        return(prepped)
        
}

# creates tidied games dataset for analysis
split_games = function(games,
                       end_train_year,
                       min_ratings) {
        
        # training set
        # removes unreleased games
        # then removes games with data quality issues (drop, missinginess on yearpublished, etc)
        train = games %>%
                # filter to year
                filter(yearpublished <= end_train_year) %>%
                # drop games that weren't released or have data quality issues
                filter(!(game_id %in% c(
                        unreleased_games$game_id))
                ) %>%
                # )))
                # filter(!(game_id %in% c(
                #         unreleased_games$game_id,
                #         drop_games$game_id))
                # ) %>%
                # filter out games with missingness on yearpublished
                filter(!is.na(yearpublished)) %>%
                # filter to games with at least X votes
                filter(usersrated >= min_ratings) %>%
                # set yearpublished to numeric
                mutate(yearpublished = as.numeric(yearpublished)) %>%
                # get description
                left_join(., 
                          game_descriptions %>%
                                  select(game_id, description),
                          by = c("game_id")) %>%
                transmute(game_id,
                          name,
                          yearpublished,
                          image,
                          thumbnail,
                          averageweight,
                          average,
                          bayesaverage,
                          usersrated,
                          minage,
                          minplayers,
                          maxplayers,
                          playingtime,
                          minplaytime,
                          maxplaytime,
                          description,
                          load_ts)
        
        # validation set
        # games published after training year
        # still excludes those with key data quality issues (meet drop criteria; missingness on yearpublished)
        # but, keeps games with missigness on average weight
        # and applies no minimum ratings filter
        valid = 
                games %>%
                filter(yearpublished > end_train_year) %>%
                # drop games that weren't released or have data quality issues
                filter(!(game_id %in% c(
                        unreleased_games$game_id,
                        drop_games$game_id))
                ) %>%
                # filter out games with missingness on yearpublished
                filter(!is.na(yearpublished)) %>%
                # set yearpublished to numeric
                mutate(yearpublished = as.numeric(yearpublished)) %>%
                # get description
                left_join(., 
                          game_descriptions %>%
                                  select(game_id, description),
                          by = c("game_id")) %>%
                transmute(game_id,
                          name,
                          yearpublished,
                          image,
                          thumbnail,
                          averageweight,
                          average,
                          bayesaverage,
                          usersrated,
                          minage,
                          minplayers,
                          maxplayers,
                          playingtime,
                          minplaytime,
                          maxplaytime,
                          description,
                          load_ts) %>%
                select(all_of(names(train)))
        
        # games not in train or valid
        other = 
                games %>%
                filter(!(game_id %in% c(train$game_id, valid$game_id))) %>%
                # get description
                left_join(., 
                          game_descriptions %>%
                                  select(game_id, description),
                          by = c("game_id")) %>%
                transmute(game_id,
                          name,
                          yearpublished,
                          image,
                          thumbnail,
                          averageweight,
                          average,
                          bayesaverage,
                          usersrated,
                          minage,
                          minplayers,
                          maxplayers,
                          playingtime,
                          minplaytime,
                          maxplaytime,
                          description,
                          load_ts) %>%
                select(all_of(names(train))) 
        
        
        return(list("train_games" = train,
                    "valid_games" = valid,
                    "other_games" = other))
        
        
}

# categorical features ----------------------------------------------------

# function for abbreviating categories
abbreviate_categorical = function(x) {
        
        tolower(gsub("\\/", "", 
                     gsub("\\)", "", 
                          gsub("\\(", "", 
                               gsub("[[:space:]]", "_", 
                                    gsub("[^[:alnum:] ]", "", x,
                                         gsub("[[:punct:]]\\s+", "", x)))))))
        
}

# function for making categorical dummies from type and value
tidy_categorical_variables = function(x) {
        
        x %>%
                mutate(abbrev= paste(substr(type, 1, 3)),
                       level = abbreviate_categorical(value)) %>%
                mutate(value = paste(abbrev, level, sep="_"),
                       has_value = 1) %>%
                select(-abbrev, -level)
}

# function to pivot categorical features
pivot_categorical_variables = function(games_categorical) {
        
        games_categorical %>%
                # tidy
                tidy_categorical_variables() %>%
                # pivot
                pivot_wider(names_from = c("value"),
                            values_from = c("has_value"),
                            values_fill = 0,
                            id_cols = c("game_id"))
        
}

# function specifically for prepping for hurdle model
# categories_and_mechanics
hurdle_prep = function(games,
                       api = F) {
        
        
        if (api == F) {
                
                # create dummies for categorical variables
                hurdle_categorical = 
                        bind_rows(game_categories,
                                  game_mechanics) %>%
                        distinct(game_id, type, id, value) %>%
                        pivot_categorical_variables()
                
                # join games with categorical dummies
                games %>%
                        select(-starts_with("cat_"),
                               -starts_with("mec_")) %>%
                        left_join(.,
                                  hurdle_categorical,
                                  by = c("game_id")) %>%
                        # replace nas
                        mutate_at(vars(starts_with("cat_"),
                                       starts_with("mec_")),
                                  ~ replace_na(., 0))
                
        } else if (api == T) {
                
                games %>%
                        # replace nas
                        mutate_at(vars(starts_with("cat_"),
                                       starts_with("mec_")),
                                  ~ replace_na(., 0))
                
                
                
        }
   
        
}

# function to create categorical variables given a training set of games
create_categorical_variables = function(train_games) {
        
        # n = 10
        # message(paste("creating features for game mechanics with at least", n, "games"))
        selected_mechanics =
                game_mechanics %>%
                # filter to games in train
                filter(game_id %in% train_games$game_id) %>%
                # min games filter
                group_by(id) %>%
                mutate(n_games = n_distinct(game_id)) %>%
                ungroup() %>%
                distinct(type, id, value)
        
        # categories
        # message(paste("creating features for game categories with at least", n, "games"))
        selected_categories = 
                game_categories %>%
                # filter games in training
                filter(game_id %in% train_games$game_id) %>%
                # min games filter
                group_by(id) %>%
                mutate(n_games = n_distinct(game_id)) %>%
                ungroup() %>%
                distinct(type, id, value)
        
        # families
        n = 100
        message(paste("creating features for selected game families with at least", n, "games"))
        selected_families = 
                game_families %>%
                # filter games in training
                filter(game_id %in% train_games$game_id) %>%
                # remove issues of leakage
                filter(!grepl("Admin Better Description", value)) %>%
                filter(!grepl("Digital Implementations", value)) %>%
                filter(!grepl("Misc", value)) %>%
                filter(!grepl("Unreleased", value)) %>%
                filter(!grepl("Upcoming Releases", value)) %>%
                filter(!grepl("Components Game Trayzinside", value)) %>%
                filter(!grepl("Spieleschmiede|Verkami|Indiegogo", value)) %>%
                # min games filter
                group_by(id) %>%
                mutate(n_games = n_distinct(game_id)) %>%
                ungroup() %>%
                filter(n_games > n) %>%
                distinct(type, id, value)
        
        # estimates for designers and artists to reduce the cardinality
        message("fitting a lasso to select game designers and artists...")
        designer_artist_effects = 
                estimate_partial_effects(train_games,
                                         bind_rows(game_designers,
                                                   game_artists),
                                         outcome = 'bayesaverage',
                                         min_coef = .25,
                                         min_games = 10) %$%
                partial_effects
        
        # get list of designers
        designer_list = 
                designer_artist_effects %>%
                filter(type == 'designer') %>%
                pull(id)
        
        # get list of artists
        artist_list = 
                designer_artist_effects %>%
                filter(type == 'artist') %>%
                pull(id)
        
        # designers
        selected_designers = 
                game_designers %>%
                # filter to designers in list
                filter(id %in% designer_list) %>%
                distinct(type, id, value)
        
        # artists
        selected_artists = 
                game_artists %>%
                # filter to artists in list
                filter(id %in% artist_list) %>%
                distinct(type, id, value)
        
        # publishers
        message("creating features for publishers on allow list...")
        selected_publishers = 
                game_publishers %>%
                # allow list
                filter(id %in% publisher_allow_list) %>%
                distinct(type, id, value)
        
        
        # bind selections together
        categorical_selected = 
                bind_rows(selected_designers,
                          selected_categories,
                          selected_mechanics,
                          selected_publishers,
                          selected_families,
                          selected_artists) %>%
                select(type, id, value) %>%
                mutate(include = 1)
        
        # pivot games
        games_categorical_pivoted = 
                # bind games and categorical togther
                bind_rows(game_designers,
                          game_categories,
                          game_mechanics,
                          game_publishers,
                          game_families,
                          game_artists) %>%
                select(game_id, type, id, value) %>%
                # join up with those selected; keep only ones we've marked to include
                left_join(.,
                          categorical_selected,
                          by = c("type", "id", "value")) %>%
                # keep only those we selected
                filter(include == 1) %>%
                select(game_id, type, id, value) %>%
                # now pivot
                pivot_categorical_variables() 
        
        # make mapping
        categorical_mapping = 
                categorical_selected %>%
                select(type, id, value) %>%
                distinct %>%
                mutate(tidied = abbreviate_categorical(value)) %>%
                select(type, id, value, tidied)
        
        message("done.")
        
        
        out = list("games_categorical_pivoted" = games_categorical_pivoted,
                   "categorical_mapping" = categorical_mapping)
        
        return(out)
        
}

# process dataset with categorical
preprocess_categorical_games = function(games) {
        
        # some simple feature engineering
        games %>%
                # number of mechanics
                mutate(number_mechanics = rowSums(dplyr::across(starts_with("mec_")))) %>%
                # mumber of categories
                mutate(number_categories = rowSums(dplyr::across(starts_with("cat_")))) %>%
                # solo game
                # big box/deluxe/anniversary edition
                mutate(deluxe_edition = dplyr::case_when(grepl("kickstarter|big box|deluxe|mega box", tolower(name))==T ~ 1,
                                                         TRUE ~ 0)) %>%
                # word count
                mutate(word_count = stringi::stri_count_words(description)) %>%
                mutate(word_count = tidyr::replace_na(word_count, 0)) %>%
                # magical phrase in description
                mutate(description_from_publisher = dplyr::case_when(grepl("description from publisher", tolower(description))==T ~ 1,
                                                                     TRUE ~ 0)) %>%
                # missingness
                mutate(missing_minage = dplyr::case_when(is.na(minage) ~ 1,
                                                         TRUE ~ 0)) %>%
                mutate(missing_playingtime = dplyr::case_when(is.na(playingtime) ~ 1,
                                                              TRUE ~ 0))
}


# function to apply transformations to bgg games data from api to get specified tables
# expects object from get_bgg_games_data bgg api function
transform_bgg_games_data = function(bgg_games_obj) {
        
        # specify games data
        bgg_games_data = bgg_games_obj$bgg_games_data
        
        # specify timestamp
        timestamp = bgg_games_obj$timestamp
        
        # links associated with games
        game_links = 
                bgg_games_data %>%
                filter(type == 'boardgame') %>%
                mutate(row = row_number()) %>%
                select(game_id, links) %>%
                mutate(is_df = map_lgl(links, ~ is.data.frame(.x))) %>%
                filter(is_df == T) %>%
                select(game_id, links) %>%
                unnest(links) %>%
                transmute(game_id,
                          type = gsub("boardgame", "", type),
                          id,
                          value,
                          load_ts = timestamp)
        
        # game names
        game_names = 
                bgg_games_data %>%
                filter(type == 'boardgame') %>%
                select(game_id, names) %>%
                unnest(names) %>%
                type_convert() %>%
                transmute(game_id,
                          type,
                          value,
                          sortindex,
                          load_ts = timestamp)
        
        # game ids
        game_ids = bgg_games_data %>%
                filter(type == 'boardgame') %>%
                select(game_id) %>%
                left_join(., 
                          game_names %>%
                                  filter(type == 'primary') %>%
                                  transmute(game_id,
                                            name = value),
                          by = 'game_id') %>%
                # get yearpublished
                transmute(game_id,
                          name,
                          load_ts = timestamp)
        
        # expansion ids
        game_expansions = game_links %>%
                filter(type == 'expansion') %>%
                transmute(game_id, 
                          expansion_id = id,
                          expansion_name = value,
                          load_ts = timestamp)
        
        # descriptions
        game_descriptions = 
                bgg_games_data %>%
                filter(type == 'boardgame') %>%
                select(game_id, info) %>%
                unnest(info) %>%
                type_convert() %>%
                transmute(game_id,
                          description,
                          load_ts = timestamp)
        
        # images
        game_images = 
                bgg_games_data %>%
                filter(type == 'boardgame') %>%
                select(game_id, info) %>%
                unnest(info) %>%
                type_convert() %>%
                # remove missingness
                filter(!is.na(image)) %>%
                transmute(game_id,
                          image,
                          thumbnail,
                          load_ts = timestamp)
        
        # player counts
        game_playercounts = 
                bgg_games_data %>%
                filter(type == 'boardgame') %>%
                select(game_id, polls) %>%
                unnest(polls) %>%
                type_convert() %>%
                transmute(game_id,
                          value,
                          numvotes,
                          numplayers,
                          load_ts = timestamp)
        
        # game ranks
        game_ranks = 
                bgg_games_data %>%
                filter(type == 'boardgame') %>%
                select(game_id, ranks) %>%
                unnest(ranks) %>%
                filter(name %in% c("abstracts",
                                   "boardgame", 
                                   "childrensgames",
                                   "cgs",
                                   "familygames",
                                   "partygames",
                                   "strategygames",
                                   "thematic",
                                   "wargames")) %>%
                mutate(bayesaverage = case_when(bayesaverage == 'Not Ranked' ~ NA_character_,
                                                TRUE ~ bayesaverage),
                       value = case_when(value == 'Not Ranked' ~ NA_character_,
                                         TRUE ~ value)) %>%
                type_convert() %>%
                transmute(game_id,
                          rank_type = name,
                          rank = value,
                          bayesaverage,
                          load_ts = timestamp)
        
        # info
        game_info = 
                bgg_games_data %>%
                filter(type == 'boardgame') %>%
                select(game_id, info, statistics) %>%
                unnest(c(info, statistics)) %>%
                # add names
                left_join(.,
                          game_names %>%
                                  filter(type == 'primary') %>%
                                  transmute(game_id,
                                            name = value),
                          by = c("game_id")) %>%
                transmute(game_id,
                          name,
                          yearpublished,
                          averageweight,
                          average,
                          bayesaverage,
                          usersrated,
                          stddev,
                          minage,
                          minplayers,
                          maxplayers,
                          playingtime,
                          minplaytime,
                          maxplaytime,
                          numcomments,
                          numweights,
                          owned,
                          trading,
                          wanting,
                          wishing,
                          load_ts = timestamp
                )
        
        # game playercounts
        game_rec_playercounts = 
                game_playercounts %>% 
                mutate(value = tolower(gsub("\\s+", "", value))) %>%
                filter(numvotes > 0) %>%
                group_by(game_id) %>%
                mutate(total_votes = sum(numvotes)) %>%
                ungroup() %>%
                group_by(game_id, numplayers) %>%
                slice_max(numvotes, n=1, with_ties = F) %>% 
                group_by(game_id) %>%
                select(game_id, value, numplayers, total_votes, load_ts) %>%
                ungroup() %>%
                pivot_wider(values_from = c("numplayers"),
                            names_from = c("value"), 
                            id_cols = c("game_id", "total_votes", "load_ts"),
                            names_prefix = c("playercount_"),
                            values_fn = ~ paste(.x, collapse=","))
        
        # combine
        analysis_games = 
                game_info %>%
                # get image
                left_join(.,
                          game_images %>%
                                  select(game_id,
                                         image,
                                         thumbnail),
                          by = c("game_id")) %>%
                # output
                transmute(
                        game_id,
                        name,
                        yearpublished,
                        image, 
                        thumbnail,
                        averageweight,
                        average,
                        bayesaverage,
                        usersrated,
                        stddev,
                        minage,
                        minplayers,
                        maxplayers,
                        playingtime,
                        minplaytime,
                        maxplaytime,
                        numcomments,
                        numweights,
                        owned,
                        trading,
                        wanting,
                        wishing,
                        load_ts) %>%
                mutate_at(
                        vars(c("yearpublished",
                               "averageweight",
                               "average",
                               "bayesaverage",
                               "usersrated",
                               "stddev",
                               "minplayers",
                               "maxplayers",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime")),
                        ~ na_if(., 0))
        
        # return tables
        return(list("game_info" = analysis_games,
                    "game_links" = game_links,
                    "game_names" = game_names,
                    "game_ids" = game_ids,
                    "game_expansions" = game_expansions,
                    "game_images" = game_images,
                    "game_descriptions" = game_descriptions,
                    "game_playercounts" = game_playercounts,
                    "game_rec_playercounts" = game_rec_playercounts,
                    "game_ranks" = game_ranks))
        
}

# function to take an individual game and pivot all categorical features
make_bgg_games_features = function(bgg_games_tables) {
        
        game_mechanics =
                bgg_games_tables$game_links %>%
                filter(type == "mechanic") %>%
                distinct(game_id, type, id, value)
        
        # categories
        game_categories = 
                bgg_games_tables$game_links %>%
                filter(type == "category") %>%
                distinct(game_id, type, id, value)
        
        # families
        game_families = 
                bgg_games_tables$game_links %>%
                filter(type == "family") %>%
                # remove issues of leakage
                filter(!grepl("Admin Better Description", value)) %>%
                filter(!grepl("Digital Implementations", value)) %>%
                filter(!grepl("Misc", value)) %>%
                filter(!grepl("Unreleased", value)) %>%
                filter(!grepl("Upcoming Releases", value)) %>%
                filter(!grepl("Components Game Trayzinside", value)) %>%
                distinct(game_id, type, id, value)
        
        # designers
        game_designers = 
                bgg_games_tables$game_links %>%
                filter(type == 'designer') %>%
                distinct(game_id, type, id, value)
        
        # artists
        game_artists = 
                bgg_games_tables$game_links %>%
                filter(type == 'artist') %>%
                distinct(game_id, type, id, value)
        
        # publishers
        game_publishers = 
                bgg_games_tables$game_links %>%
                filter(type == 'publisher') %>%
                distinct(game_id, type, id, value)
        
        # bind selections together and pivot
        categorical_games = 
                bind_rows(game_designers,
                          game_categories,
                          game_mechanics,
                          game_publishers,
                          game_families,
                          game_artists) %>%
                select(game_id, type, id, value) %>%
                distinct(game_id, type, id, value)
        
        # pivot games
        categorical_games_pivoted = 
                categorical_games %>%
                select(game_id, type, id, value) %>%
                # now pivot
                pivot_categorical_variables() 
        
        # now join with game info
        game_features = 
                bgg_games_tables$game_info %>%
                left_join(.,
                          categorical_games_pivoted,
                          by = c("game_id"))
        
        # make mapping
        categorical_mapping = 
                categorical_games %>%
                select(game_id, type, id, value) %>%
                distinct %>%
                mutate(tidied = abbreviate_categorical(value)) %>%
                select(game_id, type, id, value, tidied)
        
        # return
        out = list("game_features" = game_features,
                   "categorical_mapping" = categorical_mapping)
        
        return(out)
        
}

# not run
# bgg_games_obj = get_bgg_games_data(c(12, 7, 463))
# 
# bgg_games_tables = transform_bgg_games_data(bgg_games_obj)
# 
# game_features = make_bgg_games_features(bgg_games_tables)





# estimating partial effects ----------------------------------------------

# for a categorical games dataset (game_designers, game_publishers, ...):
# estimate the partial effect of each variable on a bgg outcome

# outcome can be one of ('bayesaverage', 'average', 'averageweight', or 'usersrated')
# train year is the last year used in training
# min games is the minimum number of games associated with a category

# requires games on which to train models:
# games_train

# then, makes use of categorical datasets:
# game_publishers
# game_artists

estimate_partial_effects = function(games,
                                    games_categorical,
                                    outcome = 'bayesaverage',
                                    min_coef = 0.45,
                                    min_games = 5) {
        
        # make symbolic
        # message number of games included
        message(paste(nrow(games), "games in dataset through", max(games$yearpublished, na.rm=T)))
        
        # message number of games after filtering
        message(paste(nrow(games %>% filter(!is.na(!!rlang::sym(outcome)))), "games without missingness on", outcome))
        
        # set minimum number of games by category
        message("preparing data...")
        games_categorical = 
                games_categorical %>%
                filter(game_id %in% games$game_id) %>%
                group_by(type, id, value) %>%
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
        
        # now join games with outcome
        data = 
                games %>%
                # outcome cannot be NA
                filter(!is.na(!!rlang::sym(outcome)))%>%
                select(game_id, name, yearpublished, averageweight, average, bayesaverage, usersrated) %>%
                left_join(.,
                          games_categorical_pivoted,
                          by = c("game_id")
                ) %>%
                # replace missingness in designer with zero
                mutate_if(is.numeric, replace_na, 0)
        
        # modeling ----------------------------------------------------------------
        
        # model
        # lasso 
        glmnet_reg_mod = parsnip::linear_reg(mixture = 1, 
                                             penalty = tune::tune(),
                                             engine = 'glmnet')
        
        # specify grid for tuning
        glmnet_grid <- tibble(penalty = 10^seq(-3, -1, 
                                               length.out = 20))
        
        # folds for tuning
        set.seed(1999)
        folds = vfold_cv(data,
                         strata = !!outcome,
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
                step_mutate(year = dplyr::case_when(yearpublished < 1900 ~ 1900,
                                             TRUE ~ yearpublished)) %>%
                # then, add spline for truncated yearpublished
                step_ns(year,
                        deg_free = 5) %>%
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
                filter(!grepl("year_|averageweight|Intercept", term)) %>%
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
                # impose an (admittedly arbitrary) minimum coefficient to keep
                filter(abs(estimate) > abs(min_coef)) %>%
                # strip out
                mutate(type = str_sub(term, 0, 3L)) %>%
                mutate(type = case_when(type == 'des' ~ 'designer',
                                        type == 'pub' ~ 'publisher',
                                        type == 'art' ~ 'artist')) %>%
                mutate(term = str_sub(term, start = 5L, end = -1L)) %>%
                # exponentitate if using (logged) usersrated
                mutate(estimate = case_when(outcome == 'usersrated' ~ (exp(estimate)-1),
                                            TRUE ~ estimate)) %>%
                left_join(.,
                          games_categorical_mapping %>%
                                  select(type, tidied, value, id),
                          by = c('term' = 'tidied',
                                 'type' = 'type')) %>%
                select(outcome, type, id, value, term, estimate)
        
        out = list("partial_effects" = partial_effects,
                   "workflows" = wflow_fits)
        
        return(out)
        
}

# # not run
# returned = estimate_partial_effects(
#         train_games,
#         game_designers,
#         end_train_year = 2021,
#         min_ratings = 50,
#         min_games = 5)

