get_bgg_api_data <-
function(input_game_id) {
        
        require(httr)
        require(xml2)
        require(XML)
        require(rvest)
        require(purrr)
        
        # push through api
        url = GET(paste('https://www.boardgamegeek.com/xmlapi2/thing?id=', paste(input_game_id, collapse=","), '&stats=1', sep=""))
        
        # get url
        doc = xml2::read_xml(url)
        
        # parse
        parsed = xmlInternalTreeParse(doc, useInternalNodes = T)
        
        # get returned ids from this
        idNodes <- getNodeSet(parsed, "//item[@id]")
        
        # returned game_ids
        returned_game_ids <- lapply(idNodes, function(x) xmlAttrs(x)['id']) %>%
                do.call(rbind, .) %>%
                as.integer(.)

        # get thumbnails
        info_parser = function(function_ids, var) {
                
                foreach(i = 1:length(function_ids),
                        .combine = bind_rows) %do% {
                                
                                getNodeSet(parsed, "//item")[[i]][paste(var)] %>%
                                        lapply(., xmlToList) %>%
                                        do.call(rbind, .) %>%
                                        as_tibble() %>%
                                        mutate(game_id = function_ids[i]) %>%
                                        select(game_id, everything())
                                
                        
                }
                
        }
        
        ### get specific output
        game_names = info_parser(function_ids = returned_game_ids,
                                 var = 'name')
        
        # thumbnails
        game_thumbnails = info_parser(function_ids = returned_game_ids,
                                      var = 'thumbnail') %>%
                set_names(., c("game_id", "thumbnail"))
        
        # description
        game_description = info_parser(function_ids = returned_game_ids,
                                       var = 'description') %>%
                set_names(., c("game_id", "description"))
        
        # image
        game_image = info_parser(function_ids = returned_game_ids,
                                 var = 'image') %>%
                set_names(., c("game_id", "image"))
        
        # categories, mechanics, etc
        game_categories= suppressWarnings(
                info_parser(function_ids = returned_game_ids,
                            var = 'link') %>%
                        select(game_id, type, id, value)
        )
        
        ## summary info
        # summary of game
        summary_parser = function(function_ids,
                                  var) {
                
                foreach(i = 1:length(function_ids), 
                        .combine = bind_rows) %do% {
                        getNodeSet(parsed, "//item")[[i]][paste(var)] %>%
                                lapply(., xmlToList) %>%
                                do.call(rbind, .) %>% 
                                as_tibble() %>%
                                mutate(game_id = function_ids[i]) %>% 
                                mutate(type = paste(var)) %>%
                                select(game_id, everything()) %>%
                                select(game_id, type, value)
                }
                
        }
        
        # selected summary info
        summary = c("yearpublished",
                    "minage",
                    "minplayers",
                    "maxplayers",
                    "playingtime",
                    "minplaytime",
                    "maxplaytime")
        
        # get game summary
        game_summary = foreach(h = 1:length(summary),
                           #    .errorhandling = 'remove',
                               .combine = bind_rows) %do% {
                                       summary_parser(function_ids = returned_game_ids,
                                                      var = summary[h])
                               }
        
        # statistics
        stats = c("usersrated",
                  "average",
                  "bayesaverage",
                  "stddev",
                  "owned",
                  "trading",
                  "wanting",
                  "wishing",
                  "numcomments",
                  "numweights",
                  "averageweight")
        
        # function
        stats_parser = function(function_ids,
                                var) {
                foreach(i = 1:length(function_ids), 
                        .combine = bind_rows) %do% {
                        
                        getNodeSet(parsed, "//ratings")[[i]][paste(var)] %>%
                                lapply(., xmlToList) %>%
                                do.call(rbind, .) %>%
                                as_tibble() %>%
                                mutate(game_id = function_ids[i]) %>%
                                mutate(type = paste(var)) %>%
                                select(game_id, everything()) %>%
                                select(game_id, type, value)
                }
        }
        
        # get stats
        game_stats = foreach(h=1:length(stats),
                             .combine = bind_rows) %do% {
                stats_parser(function_ids = returned_game_ids,
                             var = stats[h])
        }
        
        # get ranks
        # function
        ranks_parser = function(function_ids,
                                var) {
                foreach(i = 1:length(function_ids), 
                        .errorhandling = 'remove',
                        .combine = bind_rows) %do% {
                        
                        getNodeSet(parsed, "//ranks")[[i]][paste(var)] %>%
                                lapply(., xmlToList) %>%
                                do.call(rbind, .) %>%
                                as_tibble() %>%
                                mutate(game_id = function_ids[i]) %>%
                                mutate(type = paste(var)) %>%
                                select(game_id, everything())
                }
        }
        
        # get ranks
        game_ranks = ranks_parser(function_ids =  returned_game_ids,
                                  var = 'rank')
        
        # for parsing the poll
        poll_parser = function(function_ids) {
                foreach(i = 1:length(function_ids), 
                        .combine = bind_rows) %do% {
                                
                                poll = getNodeSet(parsed, "//item")[[i]]['poll'][[1]] # getting first element from the poll
                                results = getNodeSet(poll, 'results')  %>%
                                        map(., xmlToList)
                                
                                check = length(unlist(results))
                                
                                if (check > 3) {
                                        # player counts with votes
                                        numplayers = results %>% 
                                                map(., ".attrs") %>% 
                                                do.call(rbind, .) %>% 
                                                as_tibble() %>% 
                                                pull() %>%
                                                rep(each = 3) %>%
                                                as_tibble() %>%
                                                rename(numplayers = value)
                                        
                                        # the votes
                                        votes = results %>% map(., as.data.frame) %>% 
                                                map(., t) %>%
                                                do.call(rbind, .) %>% 
                                                as_tibble() %>%
                                                filter(value %in% c("Best", "Recommended", "Not Recommended"))
                                        
                                        # combine and out
                                        return = bind_cols(
                                                numplayers,
                                                votes) %>%
                                                mutate(game_id = function_ids[i]) %>% 
                                                select(game_id, numplayers, value, numvotes) 
                                }
                                
                                else {
                                        return = tibble(game_id = function_ids[i]) 
                                }
                                
                                return
                                
                        }
                
        }
        
        # votes for each games
        game_playercounts = poll_parser(function_ids = returned_game_ids)
        
        ## pivot some of this for output
        game_features = game_summary %>%
                spread(type, value) %>%
                left_join(., 
                          game_stats %>%
                                  spread(type, value),
                          by = c("game_id"))
        
        # list of ids put in
        input_game_ids = input_game_id
        
        # list of ids not returned from input
        missing_game_ids = input_game_id[!(input_game_ids %in% returned_game_ids)]
        
        # combine output
        output = list("timestamp" = Sys.time(),
                      "input_game_ids" = input_game_ids,
                      "returned_game_ids" = returned_game_ids,
                      "missing_game_ids" = missing_game_ids,
                      "game_description" = game_description,
                      "game_names" = game_names,
                      "game_thumbnails" = game_thumbnails,
                      "game_image" = game_image,
                      "game_features" = game_features,
                      "game_playercounts" = game_playercounts,
                      "game_categories" = game_categories,
                      "game_ranks" = game_ranks)
        
        return(output)
        
}

# # save function
# dump("get_bgg_api_data",
#      file = here::here("functions/get_bgg_data_from_api.R"))
