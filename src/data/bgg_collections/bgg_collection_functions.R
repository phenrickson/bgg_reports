# get user collection data from bgg 
# via bggAnalytics

get_user_collection <-
        function(username_string) {
                
                require(bggAnalytics)
                
                # function to keep retrying on error
                retry = function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
                        
                        require(futile.logger)
                        require(utils)
                        
                        attempts = 0
                        retval = try(eval(expr))
                        while (isError(retval)) {
                                attempts = attempts + 1
                                if (attempts >= maxErrors) {
                                        msg = paste("too many attempts; stopping search")
                                        stop(msg)
                                } else {
                                        msg = paste("retry: no response in in attempt", attempts, "of",  maxErrors)
                                       # flog.error(msg)
                                        message(msg)
                                }
                                if (sleep > 0) Sys.sleep(sleep)
                                retval = try(eval(expr))
                        }
                        
                        return(retval)
                }
                
                # search for collection up to 10 times; sleep for 10 seconds between requests
                collection_obj = 
                        retry(bggCollection$new(username = username_string),
                              maxErrors = 10,
                              sleep = 10)
                
                # expand
                collection_obj$expand(variable_names = c("name",
                                                         "type",
                                                         "yearpublished",
                                                         "rating",
                                                         "numplays",
                                                         "own",
                                                         "preordered",
                                                         "prevowned",
                                                         "fortrade",
                                                         "want",
                                                         "wanttoplay",
                                                         "wanttobuy",
                                                         "wishlist",
                                                         "wishlistpriority"))
                
                # convert to dataframe
                collection_data<-collection_obj$data %>%
                        transmute(username = username_string,
                                  url = collection_obj$api_url,
                                  load_ts = collection_obj$timestamp,
                                  game_id = objectid,
                                  name,
                                  type,
                                  rating,
                                  own,
                                  preordered,
                                  prevowned,
                                  fortrade,
                                  want,
                                  wanttoplay,
                                  wanttobuy,
                                  wishlist,
                                  wishlistpriority) %>%
                        # convert logical to dummies
                        mutate_if(is.logical, ~ case_when(. == T ~ 1,
                                                          TRUE ~ 0)) %>%
                        as_tibble() %>%
                        # order by game id and then desc own
                        arrange(game_id, desc(own))
                        
                
                # check for duplicated game_ids
                dupes = which(duplicated(collection_data$game_id)==T)
                
                if (length(dupes) > 0) {
                        collection_data_out = collection_data[-dupes,]
                } else {
                        collection_data_out = collection_data
                }
       
                # convert to tibble
                collection_data_out = collection_data_out %>%
                        as_tibble()
                
                return(collection_data_out)
                
        }
