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
