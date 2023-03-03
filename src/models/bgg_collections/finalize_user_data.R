# finalize data for user modeling by dummying selected categorical variables 

# pivots selected categorical variables given data
finalize_data_for_modeling = function(data) {
        
        # get outcomes for data
        message('finalizing user data for modeling...')
        
        outcomes = 
                data %>%
                select(game_id, 
                       name, 
                       yearpublished,
                       prepped,
                       bgg_info,
                       bgg_outcomes,
                       user_outcomes,
                       images,
                       description) %>%
                # unnest outcomes
                unnest(c(bgg_outcomes, user_outcomes, images, description)) %>%
                unnest(c(prepped, bgg_info))
        
        # mechanics and categories
        message('creating dummies for mechanics and categories...')
        mechanics_categories = 
                data %>%
                select(game_id, mechanics, categories) %>%
                pivot_longer(cols = -c(game_id),
                             names_to = c("features"),
                             values_to = c("value")) %>%
                unnest(value) %>%
                pivot_categorical_variables()
        
        # publishers
        # filter to those in allow list
        message('creating dummies for selected publishers..')
        publishers = 
                data %>%
                select(game_id, publishers) %>%
                pivot_longer(cols = -c(game_id),
                             names_to = c("features"),
                             values_to = c("value")) %>%
                unnest(value) %>%
                filter(id %in% publisher_allow_list) %>%
                pivot_categorical_variables()
        
        # designers, artists, families
        # filter to those in lasso
        message('creating dummies for selected designers, artists, and families.')
        designers_artists_families = 
                data %>%
                select(game_id, designers, artists, families) %>%
                pivot_longer(cols = -c(game_id),
                             names_to = c("features"),
                             values_to = c("value")) %>%
                unnest(value) %>%
                select(game_id, features, id, value, type) %>%
                # join with selected features to filter to only these
                left_join(.,
                          selected_features %>%
                                  mutate(selected = 1),
                          by = c('id', 'value', 'type')) %>%
                filter(selected == 1) %>%
                distinct(game_id, features, value, type) %>%
                pivot_categorical_variables()
        
        # join back up
        message('creating final data set...')
        outcomes %>%
                left_join(.,
                          mechanics_categories,
                          by = c("game_id")) %>%
                left_join(.,
                          publishers,
                          by = c("game_id")) %>%
                left_join(.,
                          designers_artists_families,
                          by = c("game_id")) %>%
                mutate(username = username) %>%
                select(username, everything()) %>%
                # missingness
                mutate_at(vars(starts_with("mec_"),
                               starts_with("art_"),
                               starts_with("cat_"),
                               starts_with("pub_"),
                               starts_with("des_"),
                               starts_with("fam_")),
                          replace_na, 0) %>%
                # change zeros to missingness
                mutate_at(
                        vars(c("yearpublished",
                               "averageweight",
                               "average",
                               "bayesaverage",
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
                ) %>%
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
                mutate(word_count = as.numeric(tidyr::replace_na(word_count, 0))) %>%
                # magical phrase in description
                mutate(description_from_publisher = dplyr::case_when(grepl("description from publisher", tolower(description))==T ~ 1,
                                                                     TRUE ~ 0)) %>%
                # missingness
                mutate(missing_minage = dplyr::case_when(is.na(minage) ~ 1,
                                                         TRUE ~ 0)) %>%
                mutate(missing_playingtime = dplyr::case_when(is.na(playingtime) ~ 1,
                                                              TRUE ~ 0))
        
}

games_with_user_collection = 
        finalize_data_for_modeling(games_with_user_collection_raw)

message(paste('preprocessing for', username, 'complete.'))

gc()
