# join user collection with games

# uses games prepped and games nested to return nested dataset with game features and user collection
join_user_collection = function(games_prepped,
                                games_nested,
                                user_collection) {
        
        out = 
                games_prepped %>%
                # join with games nested
                left_join(.,
                          games_nested %>%
                                  select(game_id, 
                                         name,
                                         bgg_info, 
                                         bgg_outcomes,
                                         images, 
                                         description,
                                         categories, 
                                         mechanics, 
                                         designers, 
                                         artists, 
                                         families,
                                         publishers) %>%
                                  # unnest
                                  unnest(c(bgg_info, bgg_outcomes)) %>%
                                  # remove
                                  select(-yearpublished, -averageweight, -stddev) %>%
                                  # nest back
                                  nest(bgg_info = c(minage, minplayers, maxplayers, playingtime, minplaytime, maxplaytime),
                                       bgg_outcomes = c(average, bayesaverage, usersrated)),
                          by = c("game_id", "name")) %>%
                # join with user collection
                left_join(.,
                          user_collection %>%
                                  select(-username, -username_load_ts),
                          by = c("game_id", "name")
                ) %>%
                # remove type
                select(-type) %>%
                # convert user outcomes to factor 
                # convert to factor
                mutate_at(vars(c(own,
                                 preordered, 
                                 prevowned, 
                                 fortrade,
                                 want,
                                 wanttoplay,
                                 wanttobuy, 
                                 wishlist,
                                 rated, 
                                 ever_owned)),
                          ~ factor(
                                  case_when(. == 1 ~ 'yes',
                                            TRUE ~ 'no'),
                                  levels = c("no", "yes")
                          )
                ) %>%
                # nest user outcomes
                nest(user_outcomes = c(rating,
                                       rated,
                                       own,
                                       ever_owned,
                                       preordered,
                                       prevowned,
                                       fortrade,
                                       want,
                                       wanttoplay,
                                       wanttobuy,
                                       wishlist,
                                       wishlistpriority))

        return(out)
        
}

# use function
games_with_user_collection_raw = 
        join_user_collection(games_prepped,
                             games_nested,
                             user_collection)

rm(games_prepped)

message("games joined with user collection")




