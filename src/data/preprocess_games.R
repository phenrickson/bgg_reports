#' Applies a standardized approach for abbreviating text
#' 
#' Removes all punctuation from a character and replaces any white space with an underscore
#' Used in preprocessing categorical data from bgg for modeling
abbreviate_text= function(x) {
        
        require(stringr)
        
        x %>%
                str_to_lower %>%
                str_replace(., "-", " ") %>%
                str_replace(., ":", " ") %>%
                str_replace(., "/", " ") %>%
                str_remove_all(., "[:punct:]") %>%
                str_remove_all(., "[^[:alnum:] ]") %>%
                str_remove_all(., "\\(") %>%
                str_remove_all(., "\\)") %>%
                str_replace_all(., "\\s+", "_") %>%
                str_squish
}

#' Takes values from nested cateogorical variables
#' in nested games data and collapses the value for tokenization via textrecipes
#' 
#' Uses abbreviate_text for abbreviation
#' Collapses using a simple comma
collapse_categorical = function(data,
                                var) {
        
        var = enquo(var)
        
        # join with original data
        data %>%
                select(-!!var) %>%
                left_join(.,
                          # collapse value within nested var
                          data %>%
                                  select(game_id, !!var) %>%
                                  unnest(!!var) %>%
                                  # abbreviate
                                  mutate(value = abbreviate_text(value)) %>%
                                  # collapse
                                  group_by(game_id) %>%
                                  summarize(!!var := paste(value, collapse = ", "),
                                            .groups = 'drop'),
                          by = c("game_id")
                )
        
        
}

#' Takes values from nested games data and collapses the value for tokenization via textrecipes
#' 
#' Uses abbreviate_text for abbreviation
#' Collapses using a simple comma
collapse_value = function(data,
                          var) {
        
        var = enquo(var)
        
        # join with original data
        data %>%
                select(-!!var) %>%
                left_join(.,
                          # collapse value within nested var
                          data %>%
                                  select(game_id, !!var) %>%
                                  unnest(!!var) %>%
                                  # abbreviate
                                  mutate(value = abbreviate_text(value)) %>%
                                  # collapse
                                  group_by(game_id) %>%
                                  summarize(!!var := paste(value, collapse = ", "),
                                            .groups = 'drop'),
                          by = c("game_id")
                )
        
}

#' Process games for modeling by collapsing all specified categorical variables
#' 
process_categorical = function(data) {
        
        data %>%
                # categories
                collapse_categorical(.,
                                     var = categories) %>%
                # mechanics
                collapse_categorical(.,
                                     var = mechanics) %>%
                # families
                collapse_categorical(.,
                                     var = families) %>%
                # designers
                collapse_categorical(.,
                                     var = designers) %>%
                # artists
                collapse_categorical(.,
                                     var = artists) %>%
                # publishers
                collapse_categorical(.,
                                     var = publishers)
        
}

#' Applies all previous functions to nested table
#' 
prep_data_for_models= function(data) {
        
        data %>%
                process_categorical() %>%
                select(game_id,
                       name,
                       bgg_info,
                       bgg_outcomes,
                       bgg_community,
                       images,
                       description,
                       categories,
                       mechanics,
                       families,
                       designers,
                       artists,
                       publishers,
                       load_ts
                ) %>%
                # unnest
                unnest(c(bgg_info,
                         bgg_outcomes,
                         bgg_community,
                         images,
                         description))
        
}

# not run
# games_nested %>%
#         prep_data_for_models()





