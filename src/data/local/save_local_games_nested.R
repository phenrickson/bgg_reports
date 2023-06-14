# what: make game-level nested dataset containing all bgg features

# data

# packages ----------------------------------------------------------------
library(tidyverse)

# load analysis tables
load(here::here("data", "local", "analysis_games_tables.Rdata"))

# functions ------------------------------------------------------------------

#` Creates a nested data frame at the game level using tables from bgg
#` 
#` This function takes the analysis games table and applies some simple preprocessing
#` as well as nesting variables based on type
prep_analysis = function(data) {
        
        # required packages
        suppressMessages({
                require(tidyverse)
        })
        
        ### analysis layer
        data %>%
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
                        vars(c("yearpublished",
                               "minage",
                               "minplayers",
                               "maxplayers",
                               "usersrated",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime")),
                        as.numeric
                ) %>%
                # arrange by game id
                arrange(game_id) %>%
                # bgg_info 
                nest(bgg_info = c(yearpublished, minage, minplayers, maxplayers, playingtime, minplaytime, maxplaytime)) %>%
                # nest bgg outcomes
                nest(bgg_outcomes = c(averageweight, average, bayesaverage, usersrated, stddev)) %>%
                # nest bgg community 
                nest(bgg_community = c(numcomments, numweights, owned, trading, wanting, wishing)) %>%
                # nest images
                nest(images = c(image, thumbnail)) %>%
                # nest ranks
                nest(ranks = starts_with("rank_")) %>%
                # nest playercounts
                nest(playercounts = starts_with("playercount"))
                
}

# join nested categorical tables with analysis
nest_categorical = function(data) {
        
        # required packages
        suppressMessages({
                require(tidyverse)
                require(tidymodels)
                tidymodels_prefer()
        })
        
        data %>%
        # description
        left_join(.,
                  game_descriptions %>%
                          select(-load_ts) %>%
                          nest(description = description),
                  by = c("game_id")) %>%
        # categories
        left_join(.,
                  game_categories %>% 
                          select(-load_ts) %>%
                          nest(categories = -game_id),
                  by = c("game_id")
        ) %>%
        # mechanics
        left_join(.,
                  game_mechanics %>% 
                          select(-load_ts) %>%
                          nest(mechanics = -game_id),
                  by = c("game_id")
        ) %>%
        # designers
        left_join(.,
                  game_designers %>% 
                          select(-load_ts) %>%
                          nest(designers = -game_id),
                  by = c("game_id")
        ) %>%
        # artists
        left_join(.,
                  game_artists %>% 
                          select(-load_ts) %>%
                          nest(artists = -game_id),
                  by = c("game_id")
        ) %>%
        # families
        left_join(.,
                  game_families %>% 
                          select(-load_ts) %>%
                          nest(families = -game_id),
                  by = c("game_id")
        ) %>%
        # publishers
        left_join(.,
                  game_publishers %>% 
                          select(-load_ts) %>%
                          nest(publishers = -game_id),
                  by = c("game_id")
        ) %>%
        # implementations
        left_join(.,
                  game_implementations %>%
                          select(-load_ts) %>%
                          nest(implementations = -game_id),
                  by = c("game_id")
        ) %>%
        # compilations
        left_join(.,
                  game_compilations %>%
                          select(-load_ts) %>%
                          nest(compilations = -game_id),
                  by = c("game_id")
        )
}

# run ---------------------------------------------------------------------

# create nested dataset using functions
games_nested = 
        analysis_games %>%
        prep_analysis() %>%
        nest_categorical()

# # save local ------------------------------------------------------------------
# 
# message("saving games_nested locally")
# 
# save(games_nested,
#      file = here::here("data", "local", "games_nested.Rdata"))
