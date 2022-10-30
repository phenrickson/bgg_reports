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

# functions for connecting to gcp
source(here("functions", "connect_to_gcp.R"))

# functions for submitting a game id to bgg api
# submit game ids to 'get_bgg_game_data' function
source(here("functions", "request_games_from_bgg_api_funcs.R"))

# get bgg ids
# get most recent time game id appeared
bgg_ids<-DBI::dbGetQuery(bigquerycon, 
                         'SELECT game_id, raw_name, tidy_name, MAX(upload_ts) as most_recent_by_game FROM bgg.scraped_bgg_ids
                              GROUP BY game_id, raw_name, tidy_name')

# get vector of unique ids
bgg_ids_vec = bgg_ids %>%
        select(game_id) %>%
    #    head(500) %>%
        unique %>%
        pull

# push through function
bgg_games_xml_obj = get_bgg_game_data(bgg_ids_vec,
                        tidy = F)

# now tidy
plan(multisession, workers = 4)
system.time({
        bgg_games_data = tidy_bgg_data_xml(bgg_games_xml_obj)
        })

# message number
message(paste(length(bgg_ids_vec), "game(s) to submit to bgg api"))

# convert ids to batches to submit to api
n = 400
id_batches = split(bgg_ids_vec, ceiling(seq_along(bgg_ids_vec)/n))

message(paste("requesting game ids in batches of", n))
message(paste(length(id_batches), "batch(es) to request from api"))

# submit batches to bgg api
# loop over batches
resp_batches =  
        foreach(b = 1:length(id_batches),
                .errorhandling = 'pass') %do% 
        {
                
                message(paste("submitting batch", b, "of", length(id_batches)))
                
                # submit batch to function
                out = get_bgg_game_data(id_batches[[b]])
                
                # slight pause to avoid taxing the API
                # Sys.sleep(1)
                
                # print
                #  print(paste("batch", b, "of", length(batches), "complete"))
                cat(message(paste("batch", b, "of", length(id_batches), "complete."), sep="\n"))
                
                # return
                out
                
        }

# convert to tibble
batches_data = 
        resp_batches %>%
        bind_rows(., .id="batch")

#
message(paste("all batches submitted to api"))

# look for missing games
missing_ids = bgg_ids_vec[!(bgg_ids_vec %in% batches_data$game_id)]

# print missing games
if (length(missing_ids) == 0) {
        message("all games returned from api")
} else if (length(missing_ids) > 0) {
        warning(paste("missing", length(missing_ids), "game ids:",
                      "\n",
                      paste(missing_ids, collapse = " ")))
}

# resubmit missing games
if (length(missing_ids) > 0) {
        
        message("resubmitting missing ids through api")
        
        # send missing games through api
        missing_id_batches = split(missing, ceiling(seq_along(bgg_ids_vec)/n))
        
        # loop over missing batches
        resp_missing_batches =  
                foreach(b = 1:length(missing_id_batches),
                        .errorhandling = 'pass') %do% 
                {
                        
                        message(paste("submitting missing batch", b, "of", length(missing_id_batches)))
                        
                        # submit batch to function
                        out = get_game_data(missing_id_batches[[b]])
                        
                        # slight delay pause to avoid taxing the API
                        Sys.sleep(5)
                        
                        # print
                        #  print(paste("batch", b, "of", length(batches), "complete"))
                        cat(message(paste("batch", b, "of", length(missing_id_batches), "complete."), sep="\n"))
                        
                        # return
                        out
                        
                }
        
        # convert to tibble
        missing_batches_data = 
                resp_missing_batches %>%
                bind_rows(., .id="batch")
        
        # get data from missing batches
        
} else  {
        missing_batches_data = data.frame()
}

# combine returned with missing if any
games_data = 
        bind_rows(batches_data,
                  missing_batches_data)

message(paste("saving batches to", here("data", "api")))

# save
save(games_data,
     file = here("data", "api", paste("batches_", Sys.Date(), ".Rdata", sep=""))
     )

# done
message("get_games_from_api complete.")
