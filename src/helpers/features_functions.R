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

# function to select categorical variables given a training set of games
select_categorical_variables = function(train_games) {
        
        n = 10
        message(paste("creating features for game mechanics with at least", n, "games"))
        selected_mechanics =
                game_mechanics %>%
                # filter to games in train
                filter(game_id %in% train_games$game_id) %>%
                # min games filter
                group_by(id) %>%
                mutate(n_games = n_distinct(game_id)) %>%
                ungroup() %>%
                filter(n_games > n) %>%
                distinct(type, id, value)
        
        # categories
        message(paste("creating features for game categories with at least", n, "games"))
        selected_categories = 
                game_categories %>%
                # filter games in training
                filter(game_id %in% train_games$game_id) %>%
                # min games filter
                group_by(id) %>%
                mutate(n_games = n_distinct(game_id)) %>%
                ungroup() %>%
                filter(n_games > n) %>%
                distinct(type, id, value)
        
        # families
        n = 100
        message(paste("creating features for game families with at least", n, "games"))
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
                # min games filter
                group_by(id) %>%
                mutate(n_games = n_distinct(game_id)) %>%
                ungroup() %>%
                filter(n_games > n) %>%
                distinct(type, id, value)
        
        
        # estimates for designers and artists to reduce the cardinality
        message("fitting a lasso to create features for game designers...")
        designer_list = 
                estimate_partial_effects(train_games,
                                         game_designers,
                                         outcome = 'bayesaverage',
                                         min_games = 5) %$%
                partial_effects %>%
                pull(id)
        
        message("fitting a lasso to create features for game artists...")
        artist_list =
                estimate_partial_effects(train_games,
                                         game_artists,
                                         outcome = 'bayesaverage',
                                         min_games = 5) %$%
                partial_effects %>%
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
