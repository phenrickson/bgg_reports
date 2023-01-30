strip_punctuation = function(x) {
        
        require(stringr)
        # keep alpha numeric and spaces
        gsub("[^[:alnum:][:space:]]", "", x) %>%
                # remove ascii
                gsub("[^\u0001-\u007F]+|<U\\+\\w+>", "", .) %>%
                str_squish(.) %>%
                gsub(" ", "_", .)
        
}

pivot_categorical_features = function(categorical) {
        
        categorical %>%
                # strip punctuation
                mutate_at(c("type", "value"), 
                          ~ tolower(strip_punctuation(.))) %>%
                # combine type and value
                unite(variable, c("type", "value"), sep="_") %>%
                # flip id to 1
                mutate(value = 1) %>%
                # select what we need to keep
                select(variable,
                        value,
                        id,
                        game_id) %>%
                # pivot
                pivot_wider(names_from = c("variable", "id"),
                            values_from = c("value"),
                            values_fn = sum) %>%
                # replace_na
                mutate_all( ~ replace_na(., 0))
        
}

# implementations
# flag 
game_implementations %>%
        filter(grepl("\\<box\\>|\\<collection\\>", tolower(name))) %>%
        mutate(big_box = 1) %>%
        select(game_id, big_box)

game_implementations %>%
        filter(grepl("\\<edition\\>", tolower(name))) %>%
        mutate(new_edition = 1) %>%
        select(game_id, new_edition)

# categories

# which family features are we keeping?
# minimum n and remove specific 
pivoted_families = pivot_categorical_features(game_families %>%
                                   filter(!grepl("Admin:", value)) %>%
                                   filter(!(value %in%  c('Expansion for Base-game', 'Fan Expansion'))) %>%
                                   filter(!grepl("Digital Implementations", value)) %>%
                                   filter(!grepl("Misc:", value)) %>%
                                   filter(!grepl("Upcoming Releases", value)) %>%
                                   filter(!grepl("Components: Game Trayzinside", value)) %>% 
                                   group_by(type, family_type, family_value, value, id) %>% 
                                   add_count() %>%
                                   ungroup() %>%
                                   filter(n > 50))

# categories
pivoted_categories = pivot_categorical_features(game_categories %>%
                                                        group_by(type, value, id) %>% 
                                                        add_count() %>%
                                                        ungroup() %>%
                                                        filter(n > 50))

# artists
pivoted_artists = pivot_categorical_features(game_artists %>%
                                   group_by(id) %>%
                                   add_count() %>%
                                   ungroup() %>%
                                   filter(n > 25))

# mechanics
pivoted_mechanics = pivot_categorical_features(game_mechanics)

# designers
pivoted_designers = pivot_categorical_features(game_designers %>%
                                   group_by(id) %>%
                                   add_count() %>%
                                   ungroup() %>%
                                   filter(n > 10))

# selected publishers
publisher_list = c(
        51 # Hasbo
        ,10 # Mayfair Games
        ,102 # Decision Games
        ,196 # Multi-Man Publishing
        ,396 # Alderac Entertainment Group aka AEG
        ,1027 # Days of Wonder
        ,21847 # Pandasaurus Games
        ,1001 # (web published)
        ,4 # (Self-Published)
        ,140 # Splotter Spellen
        ,157 # Asmodee
        ,34 # Ravensburger
        ,28 # Parker Brothers
        ,39 # Pegasus Speile
        ,37 # KOSMOS
        ,20 # Milton Bradley
        ,3 # Rio Grande Games
        ,538 # Z-Man Games
        ,52 # GMT Games
        # ,8923 # IELLO
        ,17 # Fantasy Flight Games
        ,5 # Avalon Hill
        ,3320 # (Unknown)
        ,597 # Eagle-Gryphon Games
        ,5400 # Matagot
        ,26 # Games Workshop Ltd
        ,47 # Queen Games
        ,11652 # Stronghold Games
        ,19 # Steve Jackson Games
        ,13 # Wizards of the Coast
        ,12024 # Cryptozoic Entertainment
        ,10754 # Plaid Hat Games
        ,21608 # CMON Global Limited
        ,108 # Gamewright
        ,221 # WizKids
        ,171 # (Public Domain)
        ,93 # Mattel, Inc
        ,25842 # Space Cowboys
        ,23202 # Stonemaier
        ,34188 # Plan  B
        ,30958 # Capstone Games
        ,22593 # Chip Theory Games
        ,17917 # Ares Games
        ,17543 # Greater Than Games
        ,28072 # Renegade Games
        ,34846 # Restoration Games
        ,29313 # Osprey Games
        ,21765 # Roxley
        ,7345 # Czech Games Edition
        ,29412 # Awaken Realms
        ,3929 # Compass Games
        ,26991 # Button Shy
        ,2456 # The Game Crafter
        ,12 # Cheapass Games
)

# game publishers
pivoted_publishers = pivot_categorical_features(game_publishers %>%
                                   filter(id %in% publisher_list))

# combine
analysis_games %>%
        # remove drop games
        filter(! (game_id %in% drop_games$game_id)) %>%
        # minimum usersrated
        filter(usersrated >=50) %>%
        transmute(
                game_id,
                name,
                yearpublished,
                averageweight,
                average,
                bayesaverage,
                usersrated,
                minage,
                minplayers,
                maxplayers,
                minplaytime,
                maxplaytime
        ) %>%
        # now join up with pivoted 
        # families
        left_join(.,
                  pivoted_families,
                  by = c("game_id")) %>%
        # categories
        left_join(.,
                  pivoted_categories,
                  by = c("game_id")) %>%
        # mechanics
        left_join(., 
                  pivoted_mechanics,
                  by = c("game_id")) %>%
        # designers
        left_join(., 
                  pivoted_designers,
                  by = c("game_id")) %>%
        # artists
        left_join(., 
                  pivoted_artists,
                  by = c("game_id")) %>%
        # publishers
        left_join(.,
                  pivoted_publishers,
                  by = c("game_id")) %>%
        # now clean up missingness in these categorical  features
        mutate_at(
                vars(
                        starts_with("family_"),
                        starts_with("category_"),
                        starts_with("mechanic_"),
                        starts_with("designer_"),
                        starts_with("artist_"),
                        starts_with("publisher_")),
                ~ replace_na(., 0)
        ) %>%
        View()
