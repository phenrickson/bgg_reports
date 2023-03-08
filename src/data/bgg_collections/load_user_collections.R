# get user collections

# get user collection via function
user_collection = get_user_collection(username)

# loop over list
# user_collection = map(username,
#                       get_user_collection) %>%
#         bind_rows %>%
#         nest(data = -username)

user_outcomes = user_collection %>%
        # has user ever rated a game?
        mutate(rated = case_when(!is.na(rating) ~ 1,
                                 TRUE ~ 0)) %>%
        # has user ever owned a game?
        mutate(ever_owned = case_when(own == 1 | prevowned == 1 ~ 1,
                                      TRUE ~ 0)) %>%
        # rename load_ts
        rename(user_load_ts = load_ts)

rm(user_collection)


# join with user collection -----------------------------------------------

# join with full dataset of games-----------------------------------------------------------

games_with_user_outcomes_raw = 
        games_prepped %>%
        # join with user outcomes
        full_join(.,
                  user_outcomes,
                  by = c("game_id", "name")
        ) %>%
        # replace missingness in user outcomes with 0s
        mutate_at(vars(own, preordered, prevowned, fortrade, want, wanttoplay, wanttobuy, rated, ever_owned),
                  replace_na, 0)
# 

# training set for user model
train_games = 
        games_with_user_outcomes_raw %>%
        # filter to games published through end train year
        filter(yearpublished <= end_train_year) %>%
        # keep only games that meet conditions
        # have a geek rating (is not null for bayesaverage)
        # the user has rated
        # the user has ever owned
        filter(!is.na(bayesaverage) | rated != 0 | ever_owned != 0) %>%
        # reorder
        select(username, everything())

test_games = 
        games_with_user_outcomes_raw %>%
        # filter to games published through end train year
        filter(yearpublished > end_train_year) %>%
        select(username, everything())

rm(games_prepped)



        

        

