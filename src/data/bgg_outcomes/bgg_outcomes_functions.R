# function needed for predictiong with bgg outcomes

# features
source(here::here("src", "features", "make_features_functions.R"))

# averageweight
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

# bayesaverage given predicted  usersrated and average
estimate_bayesaverage = function(data,
                                 votes = 2000) {
        data %>%
                mutate(.pred_bayesaverage = ((2000*5.5)+exp(usersrated)*average) /
                               (2000 + exp(usersrated)))
        
}

# function to prep incoming games for models
convert_to_model_prototype = function(data,
                              prototype) {
        
        # mechanics
        selected_mechanics =
                game_mechanics %>%
                filter(game_id %in% data$game_id) %>%
                distinct(type, id, value)
        
        # categories
        selected_categories = 
                game_categories %>%
                filter(game_id %in% data$game_id) %>%
                distinct(type, id, value)
        
        # families
        selected_families = 
                game_families %>%
                filter(game_id %in% data$game_id) %>%
                # remove issues of leakage
                filter(!grepl("Admin Better Description", value)) %>%
                filter(!grepl("Digital Implementations", value)) %>%
                filter(!grepl("Misc", value)) %>%
                filter(!grepl("Unreleased", value)) %>%
                filter(!grepl("Upcoming Releases", value)) %>%
                filter(!grepl("Components Game Trayzinside", value)) %>%
                distinct(type, id, value)

        # designers
        selected_designers = 
                game_designers %>%
                filter(game_id %in% data$game_id) %>%
                distinct(type, id, value)
        
        # artists
        selected_artists = 
                game_artists %>%
                filter(game_id %in% data$game_id) %>%
                distinct(type, id, value)
        
        # publishers
        selected_publishers = 
                game_publishers %>%
                filter(game_id %in% data$game_id) %>%
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
        
        # pivot and select categorical features
        categorical_features = 
                # bind games and categorical togther
                bind_rows(game_designers,
                          game_categories,
                          game_mechanics,
                          game_publishers,
                          game_families,
                          game_artists) %>%
                filter(game_id %in% data$game_id) %>%
                select(game_id, type, id, value) %>%
                # join up with those selected; keep only ones we've marked to include
                left_join(.,
                          categorical_selected,
                          by = c("type", "id", "value")) %>%
                # keep only those we selected
                filter(include == 1) %>%
                select(game_id, type, id, value) %>%
                # tidy names
                tidy_categorical_variables() %>%
                # filter to only those in prototype
                filter(value %in% names(prototype)) %>%
                # pivot
                pivot_wider(names_from = c("value"),
                            values_from = c("has_value"),
                            values_fill = 0,
                            id_cols = c("game_id")) %>%
                # then bind with prototype
                bind_rows(prototype,
                          .) %>%
                # keep only categorical
                select(game_id,
                       starts_with("des_"),
                       starts_with("mec_"),
                       starts_with("art_"),
                       starts_with("cat_"),
                       starts_with("pub_"),
                       starts_with("fam_"))
        
        # join with original data
        out = 
                data %>%
                # template
                # filter out games with missingness on yearpublished
                filter(!is.na(yearpublished)) %>%
                # set yearpublished to numeric
                mutate(yearpublished = as.numeric(yearpublished)) %>%
                # get description
                left_join(., 
                          game_descriptions %>%
                                  select(game_id, description),
                          by = c("game_id")) %>%
                # template
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
                # join with categorical features
                left_join(.,
                          categorical_features,
                          by = c("game_id")) %>%
                # replace NAs in any of the categorical dummies with 0s
                mutate_at(vars(any_of(names(categorical_features))),
                          replace_na, 0) %>%
                # apply preprocessing
                preprocess_categorical_games() %>%
                # log transform usersrated
                mutate(log_usersrated = log1p(usersrated))
        
        return(out)
        
        
}



               





