# get user collection and join with processed games

# pulls user's collection from bgg; joins with processed games for modeling
# requires games_prepped from preprocess_games

load_user_collection = function(username) {
        
        # get user collection via function
        user_collection = get_user_collection(username) %>%
                # set specific schema here
                transmute(username,
                          game_id, 
                          type,
                          name,
                          rating,
                          # has user ever rated a game?
                          rated = dplyr::case_when(!is.na(rating) ~ 1,
                                                   TRUE ~ 0),
                          own,
                          # has user ever owned a game?
                          ever_owned = dplyr::case_when(own == 1 | prevowned == 1 ~ 1,
                                    TRUE ~ 0),
                          preordered,
                          prevowned,
                          fortrade,
                          want,
                          wanttoplay,
                          wanttobuy,
                          wishlist,
                          wishlistpriority,
                          username_load_ts = load_ts)
        
        # check if collection is greater than zero; aka, the function returned results for the user
        if (nrow(user_collection) <= 0 | is.null(nrow(user_collection))) {
                error("no games in user collection")
        }
        
        # get number of games listed as ever owned
        user_collection_size = user_collection %>%
                summarize(size = sum(ever_owned))
        
        # warning if too few
        if (user_collection_size <=30) {
                warning("user collection may be too small for modeling approach")
        }
        
        return(user_collection)
        
}

# use function
user_collection = 
        load_user_collection(username)

message('user collection loaded.')

# # training set for user model
# train_games = 
#         games_with_user_collection_raw %>%
#         # filter to games published through end train year
#         filter(yearpublished <= end_train_year) %>%
#         # keep only games that meet conditions
#         # have a geek rating (is not null for bayesaverage)
#         # the user has rated
#         # the user has ever owned
#         filter(!is.na(bayesaverage) | rated != 0 | ever_owned != 0) %>%
#         # reorder
#         select(username, everything())
# 
# # upcoming games; no filtering applied
# test_games = 
#         games_with_user_collection_raw %>%
#         # filter to games published through end train year
#         filter(yearpublished > end_train_year) %>%
#         select(username, everything())
# 
# 




        

        

