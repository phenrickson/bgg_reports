# source function for reading data 
source(here::here("functions/get_bgg_data_from_github.R"))
source(here::here("scripts/load_packages.R"))

# connect to bigquery
library(magrittr)
library(odbc)
library(bigrquery)
#library(bigQueryR)
library(DBI)
library(keyring)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

bq_auth(email = 'phil.henrickson@aebs.com')

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)


# get bgg ids from most recent load
all_game_ids<-DBI::dbGetQuery(bigquerycon, 
                              'SELECT game_id, type, MAX(date) as most_recent_by_game FROM bgg.api_all_game_ids
                              GROUP BY game_id, type')

# pull ids
bgg_ids = all_game_ids %>%
        filter(type == 'boardgame') %>%
        select(game_id) %>%
        unique() %>%
        pull()

# get previously saved function for getting bgg data from api
source(here::here("functions/get_bgg_data_from_api.R"))
        
# create batches of n size
n = 500
batches = split(bgg_ids, ceiling(seq_along(bgg_ids)/n))

batches_returned = foreach(b = 1:length(batches),
                           .errorhandling = 'pass') %do% {
        
        # push batch 
        out = get_bgg_api_data(batches[[b]])
        
        # pause to avoid taxing the API
        Sys.sleep(18)
        
        # print
      #  print(paste("batch", b, "of", length(batches), "complete"))
        cat(paste("batch", b, "of", length(batches), "complete"), sep="\n")
        
        # return
        out
        
        }

# indicate initial batch is complete
beepr::beep(1)

# find games not returned
check_ids = map(batches_returned, "game_features") %>%
        rbindlist(.)

# any games in this?
all_game_ids$game_id[!(all_game_ids$game_id %in% check_ids$game_id)]
        
# check the lengths of each batch
check = data.frame(length = lengths(batches_returned),
           batch = seq(batches_returned))

# batches with prpblems
problems = check %>%
        filter(length!=12) %>%
        pull(batch)

# check length
length(problems) < 1

# # rerun problem batches
batches_problems = foreach(b = 1:length(problems),
                           .errorhandling = 'pass') %do% {

                                   # push batch
                                   out = get_bgg_api_data(batches[[problems[b]]])

                                   # pause to avoid taxing the API
                                   Sys.sleep(20)

                                   # print
                                   #  print(paste("batch", b, "of", length(batches), "complete"))
                                   cat(paste("batch", b, "of", length(problems), "complete"), sep="\n")

                                   # return
                                   out

                           }


# combine to ensure we have data for every batch
batches_all = c(batches_returned,
                batches_problems)
#
# fix
for (i in 1:length(problems)) {
        batches_returned[[problems[i]]] = batches_problems[[i]]
}

#batches_returned[[problems]] = batches_problems[[1]]
# # check again
# check_again = data.frame(length = lengths(batches_returned),
#                    batch = seq(batches_returned)) %>%
#         filter(length ==9) %>%
#         nrow()
# 
# # assert that we have the right length for all batches
# assertthat::are_equal(length(batches_returned),
#                       check_again)

# # get last problem batch
# # batches with prpblems
# problems_again = data.frame(length = lengths(batches_returned),
#                             batch = seq(batches_returned)) %>%
#         filter(length !=9) %>%
#         pull(batch)
# 
# # rerun problem batches
# batches_problems_again = foreach(b = 1:length(problems_again),
#                            .errorhandling = 'pass') %do% {
#                                    
#                                    # push batch 
#                                    out = get_bgg_api_data(batches[[problems_again[b]]][-120])
#                                    
#                                    # pause to avoid taxing the API
#                                    Sys.sleep(20)
#                                    
#                                    # print
#                                    #  print(paste("batch", b, "of", length(batches), "complete"))
#                                    cat(paste("batch", b, "of", length(problems_again), "complete"), sep="\n")
#                                    
#                                    # return
#                                    out
#                                    
#                            }
# 
# # check again, again
# for (i in 1:length(problems_again)) {
#         batches_returned[[problems_again[i]]] = batches_problems_again[[i]]
# }
# 
# # assert that we have the right length for all batches
# assertthat::are_equal(length(batches_returned),
#                       data.frame(length = lengths(batches_returned),
#                                 batch = seq(batches_returned)) %>%
#                               filter(length ==9) %>%
#                               nrow())

# pulling from API done
print(paste("saving to local"))

# save the raw output if need be
readr::write_rds(batches_returned,
                 file = here::here(paste("raw/batches_returned_", Sys.Date(), ".Rdata", sep="")))

beepr::beep(3)

# extract tables
print(paste("creating tables"))

# categories
game_categories = map(batches_returned, "game_categories") %>%
        rbindlist(.) %>%
        as_tibble() %>%
        type_convert() %>%
        mutate(id = as.integer(id),
               type = gsub("boardgame", "", type))

# playercounts
game_playercounts = map(batches_returned, "game_playercounts") %>%
        rbindlist(.) %>%
        as_tibble() %>%
        type_convert()

# descriptions
game_descriptions = map(batches_returned, "game_description") %>%
        rbindlist(.) %>%
        as_tibble()

# names
game_names = map(batches_returned, "game_names") %>%
        rbindlist(.) %>%
        as_tibble() 

# images
game_images =  map(batches_returned, "game_image") %>%
        rbindlist(.) %>%
        as_tibble() %>%
        left_join(., 
                  map(batches_returned, "game_thumbnails") %>%
                          rbindlist(.) %>%
                          as_tibble(),
                  by = c("game_id"),
        )

# features
game_features = map(batches_returned, "game_features") %>%
        rbindlist(.) %>%
        as_tibble() %>%
        type_convert() %>%
        select(game_id,
               yearpublished,
               averageweight,
               average,
               bayesaverage,
               usersrated,
               stddev,
               minage,
               minplayers,
               maxplayers,
               playingtime,
               minplaytime,
               maxplaytime,
               numcomments,
               numweights,
               owned,
               trading,
               wanting,
               wishing
        )

# ranks
game_ranks = map(batches_returned, "game_ranks") %>%
        rbindlist(.) %>%
        as_tibble()  %>%
        filter(name %in% c("boardgame", 
                           "childresngames",
                           "cgs",
                           "familygames",
                           "partygames",
                           "strategygames",
                           "thematic",
                           "wargames")) %>%
        mutate(bayesaverage = case_when(bayesaverage == 'Not Ranked' ~ NA_character_,
                                        TRUE ~ bayesaverage)) %>%
        select(game_id, name, value, bayesaverage) %>%
        type_convert() %>%
        rename(rank = value) %>%
        select(-bayesaverage) %>%
        pivot_wider(id_cols = c("game_id"),
                    names_from = c("name"),
                    names_prefix = c("rank_"),
                    values_from = c("rank"))

## Combine           
# combine features and ranks
game_info = game_features %>%
        # left_join(., game_ranks,
        #           by = c("game_id")) %>%
        left_join(., game_names %>%
                          filter(type == 'primary') %>%
                          select(game_id, value) %>%
                          rename(name = value),
                  by = c("game_id")) %>%
        select(game_id,
               name, 
               everything()) %>%
        mutate(timestamp = Sys.time())

# categories
print(paste("now writing to bigquery"))

## push to bigquery
### push to GCP 
# library bigrquery
library(bigrquery)
#library(bigQueryR)
library(DBI)

bq_auth(email = 'phil.henrickson@aebs.com')

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# game categories
dbWriteTable(bigquerycon,
             name = "api_game_categories",
             overwrite = T,
             value = game_categories)

print(paste("game_categories loaded"))

# game names
dbWriteTable(bigquerycon,
             name = "api_game_names",
             overwrite = T,
             value = game_names)

print(paste("game_names loaded"))

# game playercounts
# game names
dbWriteTable(bigquerycon,
             name = "api_game_playercounts",
             overwrite = T,
             value = game_playercounts)

print(paste("game_playercounts loaded"))

# game description
dbWriteTable(bigquerycon,
             name = "api_game_descriptions",
             overwrite = T,
             value = game_descriptions)

print(paste("game_descriptions loaded"))

# game images
dbWriteTable(bigquerycon,
             name = "api_game_images",
             overwrite = T,
             value = game_images)

print(paste("game_images loaded"))

# game info
dbWriteTable(bigquerycon,
             name = "api_game_info",
             append = T,
             value = game_info)

## all done
print(paste("done."))

rm(list=ls())
gc()
