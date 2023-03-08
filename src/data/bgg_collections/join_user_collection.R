# join user collection with games

# uses games prepped and games nested to return nested dataset with game features and user collection
join_user_collection = function(games_prepped,
                                games_nested,
                                user_collection) {
        
        # set user vars
        user_vars = c("own",
                         "preordered", 
                         "prevowned", 
                         "fortrade",
                         "want",
                         "wanttoplay",
                         "wanttobuy", 
                         "rated", 
                         "ever_owned")
        
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
                                  # nest abck
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
                # create username and username load_ts
                mutate(username = user_collection$username[1], 
                       username_load_ts = user_collection$username_load_ts[1]) %>%
                # convert user outcomes to factor 
                # convert to factor
                mutate_at(vars(all_of(user_vars)),
                          ~ factor(
                                  case_when(. == 1 ~ 'yes',
                                            TRUE ~ 'no'),
                                  levels = c("no", "yes")
                          )
                ) %>%
                # nest user outcomes
                nest(user_outcomes = c(all_of(user_vars)))
        
        return(out)
        
}

# use function
games_with_user_collection_raw = 
        join_user_collection(games_prepped,
                             games_nested,
                             user_collection)

message("games joined with user collection")




