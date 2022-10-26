# packages
require(tidyverse)
require(data.table)
require(httr2)
require(xml2)
require(XML)
require(rvest)
require(purrr)
require(here)
require(polite)
require(magrittr)
require(assertthat)

### functions
# function for getting data out of node
simple_node_parse = function(node, var) {
        
        # for a node, get selected var
        data = map(node[var],
                   # convert xml to list
                   xmlToList) %>%
                # bind elements together into a tibble
                bind_rows
        
        # now nest and set name
        out = data %>%
                nest(data = everything()) %>%
                # set name
                set_names(var)
        
        # unnest if only one row and is not names
        if (nrow(data) <=1 & ncol(data) == 1) {
                out = unnest(out, cols = everything()) %>%
                        set_names(var)
        }
        
        # return out
        return(out)
        
}

# clean up html text
unescape_html <- function(str){
        xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

# request games from bgg api app
req_bgg_api = function(input_game_ids) {

        # # if more than 250, warn
        # if (length(input_game_ids) > 250) {
        #         warning(paste("requesting", length(input_game_ids), "games from api. submitting too many games in one request may lead to problems with response."))
        # }
        
        # stop if more than 500
        if (length(input_game_ids) > 500) {
                stop(paste("requesting", length(input_game_ids), "too many games in one request may lead to issues. reduce number of games in request below", 500))
        }
        
        # if duplicated
        if (identical(input_game_ids,  unique(input_game_ids))==F) {
                # warning
                warning("duplicated game ids in request. submitting only unique game ids")
                # deduplicate
                input_game_ids = unique(input_game_ids)
        }
        
        # request to bgg api
        # submit game ids in comma delimited to api
        req = request(paste('https://www.boardgamegeek.com/xmlapi2/thing?id=', paste(input_game_ids, collapse=","), '&stats=1', sep="")) 
        
        # submit request and get response
        resp = 
                req %>%
                # throttle rate of request
                req_throttle(15/60) %>%
                # perform request
                req_perform()
        
        return(resp)
        
}

# parse xml from bgg api response
parse_bgg_api = function(input_resp_bgg_api) {
        
        # get game info from bgg api response
        # get xml from response
        resp_xml = 
                input_resp_bgg_api %>%
                resp_body_xml()
        
        # parse
        resp_parsed =
                resp_xml %>%
                xmlInternalTreeParse(useInternalNodes = T)
        
        # get nodes for individual games
        resp_nodes <- 
                getNodeSet(resp_parsed, "//item[@id]")
        
        # get attributes of xml to get game type and ids
        resp_ids =
                map(resp_nodes,
                    ~ .x %>% 
                            xmlToList %>% 
                            pluck(".attrs")) %>%
                bind_rows
        
        # create new object, game nodes, which holds ids of games
        game_nodes = resp_nodes
        names(game_nodes) = resp_ids$id
        
        return(game_nodes)
        
}

# function for getting desired data from parsed response
extract_bgg_data = function(input_parsed_bgg_api) {
        
        ### game id and name
        # game id and name
        ids = 
                input_parsed_bgg_api %>%
                xmlToList() %>%
                pluck(".attrs") %>% 
                bind_rows %>%
                set_names(., c("type", "game_id"))
        
        ### simple parse
        # names
        names = 
                simple_node_parse(input_parsed_bgg_api,
                                  'name') %>%
                # set name 
                set_names(., "names")
        
        # get primary name
        name = names %>%
                unnest(cols = "names") %>% 
                filter(type == 'primary') %>%
                transmute(name = value)
        
        # description
        description = 
                simple_node_parse(input_parsed_bgg_api,
                                  'description') %>%
                # unescape html
                mutate(description = unescape_html(description))
        
        # image
        image = 
                simple_node_parse(input_parsed_bgg_api,
                                  'image')
        
        # yearpublished
        yearpublished = 
                simple_node_parse(input_parsed_bgg_api,
                                  'yearpublished')
        
        # minplayers
        minplayers = 
                simple_node_parse(input_parsed_bgg_api,
                                  'minplayers')
        
        # maxplayers
        maxplayers = 
                simple_node_parse(input_parsed_bgg_api,
                                  'maxplayers')
        
        # playingtime
        playingtime = 
                simple_node_parse(input_parsed_bgg_api,
                                  'playingtime')
        
        # minplaytime
        minplaytime = 
                simple_node_parse(input_parsed_bgg_api,
                                  'minplaytime')
        
        # maxplaytime
        maxplaytime = 
                simple_node_parse(input_parsed_bgg_api,
                                  'maxplaytime')
        
        # min age
        minage = 
                simple_node_parse(input_parsed_bgg_api,
                                  'minage')
        
        ### custom parse
        # links: this is all categories, designers, publishers
        categories = 
                map(input_parsed_bgg_api['link'],
                    xmlToList) %>%
                bind_rows %>%
                mutate(type = gsub('boardgame', '', type)) %>%
                nest("categories" = everything())
        
        # statistics
        statistics =
                input_parsed_bgg_api[['statistics']] %>% 
                .[['ratings']] %>% 
                xmlToList %>%
                .[c(
                        'averageweight',
                        'usersrated',
                        'average',
                        'bayesaverage',
                        'stddev',
                        'median',
                        'owned',
                        'trading',
                        'wanting',
                        'wishing',
                        'numcomments',
                        'numweights'
                )] %>%
                as_tibble %>%
                nest("statistics" = everything())
        
        # ranks
        ranks =
                input_parsed_bgg_api[['statistics']] %>% 
                .[['ratings']] %>% 
                .[['ranks']] %>%
                xmlToList %>%
                bind_rows %>%
                nest("ranks" = everything())
        
        # polls
        polls = 
                map(input_parsed_bgg_api[['poll']] %>% .['results'], ~ .x %>% 
                            xmlToList %>% 
                            bind_rows %>% 
                            mutate(numplayers = max(numplayers, na.rm=T)) %>%
                            filter(!is.na(value))) %>%
                bind_rows() %>% 
                select(one_of("numplayers", "value", "numvotes")) %>% 
                nest("polls" = everything())
        
        # bind together
        game_data = 
                bind_cols(
                        ids,
                        name,
                        description,
                        image,
                        yearpublished,
                        minplayers,
                        maxplayers,
                        playingtime,
                        minplaytime,
                        maxplaytime,
                        minage,
                        names,
                        categories,
                        statistics,
                        ranks,
                        polls
                )
        
        return(game_data)
        
}

# apply previous functions in one function
get_game_data = function(input_game_ids) {
        
        message(paste("requesting data for", length(input_game_ids), "game(s) from bgg api..."))
        
        # send request
        resp_bgg_api = req_bgg_api(input_game_ids)
        
        if(resp_bgg_api$status_code == 200) {message("request status ok...")} else {stop("request status not ok")}
        
        # parse response
        parsed_bgg_api = parse_bgg_api(resp_bgg_api)
        
        message("tidying data from api request...")
        
        # extract tidy data
        game_data = map(parsed_bgg_api, extract_bgg_data) %>%
                bind_rows %>%
                mutate(request_ts = Sys.time())
        
        message("done.")
        
        # return
        return(game_data)
        
        
}

# examples of using api
# foo = get_game_data(12)

# bar = get_game_data(input_game_ids = c(12, 216710))

