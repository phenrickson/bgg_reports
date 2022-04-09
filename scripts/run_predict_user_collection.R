# run user notebook
library(tidyverse)
library(foreach)

# test 
# get user collection
get_user_collection = function(username) {
        
        # load bgg analytics
        library(bggAnalytics)
        
        # load function for grabbing collections
        source(here::here("functions/get_collection.R"))
        
        # load collection
        get_collection(username) %>%
                as_tibble()
        
}

# make function
run_user_collection = function(input_user_list,
                               input_year_end,
                               input_ratings) {
        
        rmarkdown::render(here::here("predicting_user_collections.Rmd"), 
                                               params = list(username = input_user_list,
                                                             end_training_year = input_year_end,
                                                             min_training_ratings = input_ratings),
                                               output_file =  paste(
                                                       gsub("%20", 
                                                            "_",
                                                            input_user_list),
                                                       input_year_end,
                                                       sep = "_"),
                                               output_dir = here::here(""))
}

# load in users previously modeled
user_files = list.files("/Users/phenrickson/Documents/projects/bgg/predict_user_collections/user_reports") %>%
        as_tibble() %>%
        filter(grepl(".html", value)) %>%
        separate(value, into = c("username", "leftover"), sep=".html") %>%
        select(username) %>%
        mutate(username = substr(username, 1, nchar(username)-5)) %>%
        pull(username) %>%
        unique()

# # run over specified user list
users = gsub("_", "%20", user_files)


# chgeck to see if collections are present first
# run through list
test_collections = foreach(i = 1:length(users),
                           .combine = bind_rows,
                           .errorhandling = 'pass') %do% {
                
                get_user_collection(users[i])
                
        }

# check to see who was in
# set a 30 game minimum
set.seed(1)
users_passed = test_collections %>%
        group_by(username, own) %>%
        count() %>%
        ungroup() %>%
        filter(own ==1) %>%
        filter(n > 30) %>%
        sample_n(25) %>%
        pull(username)

users_passed = "ZeeGarcia"

# run users that passed through list
foreach(i = 1:length(users_passed),
        .errorhandling = 'pass') %do% {
                
                run_user_collection(input_user_list = users_passed[i],
                                    input_year_end = 2020,
                                    input_ratings = 30)
                
                }

rm(list=ls())
gc()
.rs.restartR()
