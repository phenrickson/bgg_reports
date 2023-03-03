# filtering for categorical variables ----------------------------------------------
message("feature selection for categorical variables...")

# creates training set for feature selection (for dummies)
create_train = function(data,
                        end_train_year = end_train_year) {
        
        message(paste("creating training set for games through ", end_train_year, "...", sep=""))
        
        train_raw = 
                games_with_user_collection_raw %>%
                select(game_id, 
                       name, 
                       yearpublished,
                       bgg_outcomes, 
                       user_outcomes, 
                       families, 
                       designers, 
                       artists) %>%
                # filter to games published through end of train year
                filter(yearpublished <= end_train_year) %>%
                unnest(c(bgg_outcomes, user_outcomes)) %>%
                # filter to games that meet criterion:
                # games that have a geek rating
                # or, games that the user owns/has rated
                filter(!is.na(bayesaverage) | (rated != 'no' | ever_owned != 'no'))
        
        message(paste(nrow(train_raw), "records in training set"))
        
        return(train_raw)
        
        
}

# pivot to create dummy variables for selected categorical outcomes
create_dummies = function(data,
                          categories = c("designers", "artists", "families"),
                          outcome) {
        
        
        message(paste("creating dummy variables for designers, artists, and families above minimum n..."))
        
        
        # unnest designers, artists, and families
        unnested = 
                map(categories,
                    ~ data %>%
                            select(game_id, name, !!outcome, !!.x) %>%
                            unnest(!!.x)) %>%
                bind_rows %>%
                select(game_id, name, ever_owned,type, id, value) %>%
                filter(!grepl("Admin Better Description", value)) %>%
                filter(!grepl("Digital Implementations", value)) %>%
                filter(!grepl("Misc", value)) %>%
                filter(!grepl("Unreleased", value)) %>%
                filter(!grepl("Upcoming Releases", value)) %>%
                filter(!grepl("Components Game Trayzinside", value)) %>%
                filter(!grepl("Games with expansions", value)) %>%
                filter(!grepl("Expansions", value)) %>%
                filter(!grepl("Crowdfunding Spieleschmiede", value)) %>%
                filter(!grepl("Crowdfunding Verkami", value)) %>%
                filter(!grepl("Crowdfunding Indiegogo", value))
        
        
        # pivot to create dummies for those hitting minimum n
        filtered = 
                unnested %>%
                distinct(game_id, name, value, type) %>%
                group_by(type, value) %>%
                add_count() %>%
                ungroup() %>%
                # set min n by type
                mutate(min_n = case_when(type %in% c('artist', 'designer') ~ 15,
                                         type %in% c('family') ~ 50,
                                         TRUE ~ 50)) %>%
                # keep only those above threshold
                filter(n > min_n)
        
        # create dummmies
        dummies = 
                filtered %>%
                pivot_categorical_variables()
        
        # join back with original data
        data_dummies = 
                data %>%
                select(game_id, !!outcome) %>%
                left_join(.,
                          dummies,
                          by = c("game_id")) %>%
                # replace NAs
                mutate_if(is.numeric, replace_na, 0)
        
        message(paste("returning", nrow(data_dummies), "rows by", ncol(data_dummies), "columns"))
        
        # map dummies to original values
        mapping = 
                unnested %>%
                tidy_categorical_variables() %>%
                distinct(type, id, value) %>%
                left_join(., unnested %>%
                                  transmute(game_id,
                                            type,
                                            id,
                                            original = value),
                          by = c("type", "id")) %>%
                select(game_id, type, original, id, value)
        
        return(list("data_dummies" = data_dummies,
                    "dummies_mapping" = mapping))
        
}

# fit lasso to dummies and select 
fit_lasso = function(data,
                 outcome,
                 penalty = 0.001) {
                
                require(tidymodels)
                
                
                lasso_spec = 
                        logistic_reg(penalty = penalty,
                                     mixture =1) %>%
                        set_engine("glmnet")
                
                message('fitting lasso...')
                
                fit = 
                        lasso_spec %>%
                        fit_xy(y = data %>%
                                       select(!!outcome),
                               x = data %>%
                                       select(-game_id, -!!outcome)
                        )
                
                message('lasso complete.')
                
                return(fit)
        }

# select coefs from lasso
select_coefs = 
        function(fit) {
                
                message("tidying coefs and selecting features...")
                
                require(tidymodels)
                require(glmnet)
                
                selected_dummies = 
                        fit %>%
                        # tidy
                        tidy() %>%
                        # remove intercept
                        filter(term != '(Intercept)') %>%
                        # exponentiate and round to 3 decimal places
                        mutate(estimate = round(exp(estimate), 3))%>%
                        # remove anything equal to 1
                        filter(estimate != 1)
                
                message(paste(nrow(selected_dummies), 'features selected...'))
                
                
                return(selected_dummies)
                
        }

# one fuction to employ these subfunctions
filter_dummy_features = function(data,
                           end_train_year = end_train_year,
                           outcome,
                           penalty = 0.001) {
        
        # create training set
        train_raw = 
                create_train(games_with_user_collection_raw,
                             end_train_year)
        
        # create dataset with dummies
        created_dummies = 
                create_dummies(data = train_raw,
                               outcome = 'ever_owned')
        
        # output needed for fit
        data_dummies = created_dummies %$% 
                data_dummies
        
        # mapping 
        dummies_mapping = created_dummies %$%
                dummies_mapping
        
        # fit lasso to dataset with dummies
        fit = 
                fit_lasso(data_dummies,
                          'ever_owned')
        
        # selected coefs from lasso
        selected_coefs = 
                select_coefs(fit)
        
        
        # now join back with original, unmodified values
        message('joining dummies back with original values...')
        
        selected_features =
                selected_coefs %>%
                left_join(.,
                          dummies_mapping %>%
                                  rename(term = value),
                          by = c("term")) %>%
                distinct(type, id, term, original) %>%
                rename(value = original)
        
        return(selected_features)
        
}

# run all
selected_features = filter_dummy_features(data = games_with_user_collection_raw,
                                          end_train_year = end_train_year,
                                          outcome = 'ever_owned',
                                          penalty = 0.001)

message('dummy variable selection complete.')
