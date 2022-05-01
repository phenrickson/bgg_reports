# run user notebook
library(tidyverse)
library(foreach)
source(here::here("scripts/load_packages.R"))
source(here::here("functions/theme_phil.R"))
library(patchwork)

# connect to bigquery
library(bigrquery)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# authorize
bq_auth(email = "phil.henrickson@aebs.com")

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# query table of game info to most recent load
active_games<-DBI::dbGetQuery(bigquerycon, 
                              'SELECT * FROM bgg.api_game_info
                              where timestamp = (SELECT MAX(timestamp) as most_recent FROM bgg.api_game_info)') %>%
        select(-starts_with("rank")) %>%
        mutate(numweights = as.numeric(numweights)) %>%
        mutate_at(c("averageweight",
                    "playingtime",
                    "minplaytime",
                    "maxplaytime",
                    "yearpublished"),
                  ~ case_when(. == 0 ~ NA_real_,
                              TRUE ~ .))

# ugh, made a mistake in the schema...

# create caption for plots
my_caption = list(labs(caption = paste(paste("Data from boardgamegeek.com as of", max(as.Date(active_games$timestamp))),
                                       paste("Data and analysis at github.com/phenrickson/bgg"), sep="\n")))


# long table with game type variables
game_types= DBI::dbGetQuery(bigquerycon, 
                            'SELECT * FROM bgg.api_game_categories')

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
# user_files = list.files("/Users/phenrickson/Documents/projects/bgg/predict_user_collections/user_reports") %>%
#         as_tibble() %>%
#         filter(grepl(".html", value)) %>%
#         separate(value, into = c("username", "leftover"), sep=".html") %>%
#         select(username) %>%
#         mutate(username = substr(username, 1, nchar(username)-5)) %>%
#         pull(username) %>%
#         unique()

# or specify a list of users
# user_files = c("Gyges",
#                "ZeeGarcia",
#                "Quinns",
#                "rahdo",
#                "Watch%20It%20Played",
#                "mrbananagrabber",
#                "GOBBluth89")

user_files = c("comperio")

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
    #    sample_n(25) %>%
        pull(username)

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
