# what: make game-level dataset with tokenized categorical features containing all bgg features

# data

# load analysis tables
load(here::here("data", "local", "games_nested.Rdata"))

message('loading (local) data for games...')

# # bgg data
# # load tables used in modeling
# # local version
# load(here::here("data", "local", "analysis_games_tables.Rdata"))

# # query from gcp (and update local)
# source(here::here("pull_analysis_games_tables.R"))

# nested bgg data with all info
load(here::here("data", "local", "games_nested.Rdata"))

# publisher allow list
processed_board = board_folder(here::here("data", "processed"), versioned = T)

# read in publisher allow list
publisher_allow_list = processed_board %>% pin_read("publisher_allow_list")

# function for abbreviating categories
abbreviate_categorical = function(x) {
        
        x %>%
                str_to_lower %>%
                str_remove_all(., "[:punct:]") %>%
                str_remove_all(., "[^[:alnum:] ]") %>%
                str_replace_all(., " ", "_") %>%
                str_remove_all(., "\\(") %>%
                str_remove_all(., "\\)") %>%
                str_replace_all(., "[^[:alnum:] ]", " ") %>%
                str_replace_all(., "\\s+", "_") %>%
                str_squish
}


# games nested
foo = games_nested %>%
        select(game_id) %>%
        # categories
        left_join(.,
                  games_nested %>%
                          select(game_id, categories) %>%
                          unnest(categories) %>%
                          mutate(value = abbreviate_categorical(paste(value))) %>%
                          select(game_id, type, value) %>%
                          pivot_wider(id_cols = c("game_id"),
                                      values_from = c("value"),
                                      names_from = c("type"),
                                      values_fn = ~paste(.x, collapse = ", "))) %>%
        # mechanics
        left_join(.,
                  games_nested %>%
                          select(game_id, mechanics) %>%
                          unnest(mechanics) %>%
                          mutate(value = abbreviate_categorical(paste(value))) %>%
                          select(game_id, type, value) %>%
                          pivot_wider(id_cols = c("game_id"),
                                      values_from = c("value"),
                                      names_from = c("type"),
                                      values_fn = ~paste(.x, collapse = ", ")))

library(textrecipes)

rec = recipe(foo) %>%
        update_role(game_id,
                    new_role = "id") %>%
        # tokenize
        step_tokenize(category,
                      mechanic) %>%
        # all categories
        step_tokenfilter(category,
                         min_times = 0.0001,
                         max_times = 1,
                         percentage = T) %>%
        # mechanics appearing 
        step_tokenfilter(mechanic,
                         min_times = 0.0001,
                         max_times = 1,
                         percentage = T) %>%
        # frequency
        step_tf(category,
                mechanic,
                prefix = "d") %>%
        step_center(all_predictors()) %>%
        step_scale(all_predictors(),
                       factor = 2)
        
# mechanics correlation
rec %>%
        prep(foo) %>%
        juice() %>%
        select(-game_id) %>%
        select(starts_with("d_mechanic")) %>%
        set_names(., gsub("d_mechanic_", "", names(.))) %>%
        cor() %>%
        ggcorrplot::ggcorrplot(outline.color = 'white',
                               hc.order = T)+
        theme(axis.text.x = element_text(size = 4, angle = 90),
              axis.text.y = element_text(size = 4))

# categories correlation
rec %>%
        prep(foo) %>%
        juice() %>%
        select(-game_id) %>%
        select(starts_with("d_category")) %>%
        set_names(., gsub("d_category_", "", names(.))) %>%
        cor() %>%
        ggcorrplot::ggcorrplot(outline.color = 'white',
                               hc.order = T)+
        theme(axis.text.x = element_text(size = 4, angle = 90),
              axis.text.y = element_text(size = 4))
         
                                           
                                   
        mutate(rows = map(categories, ~ nrow(.x)))
                          .default
        unnest(categories) %>%
        mutate(value = abbreviate_categorical(paste(type, value))) %>%
        select(game_id, value) %>%
        nest(categories = value) %>%
        mutate(categories = )
        mutate(category = paste(type, value))
        mutate()
