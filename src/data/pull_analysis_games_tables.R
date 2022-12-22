# who: phil henrickson
# what: query gcp tables to get datasets of games for analysis
# when: 12/20/22

message("querying tables from gcp for analysis...")

#  packages ---------------------------------------------------------------

suppressPackageStartupMessages(
        {
                require(tidyverse)
        }
)

# resolve conflict preferences
tidymodels_prefer()


# connect to gcp ----------------------------------------------------------

source(here::here("src", "functions", "connect_to_gcp.R"))


# download tables ---------------------------------------------------------------

# get analysis games table
analysis_games <-bq_table_download(bq_project_query(PROJECT_ID,
                                                    'SELECT * FROM bgg.analysis_games')) %>%
        # change integers to numeric
        mutate_if(is.integer, as.numeric) %>%
        # change zeroes to NA
        mutate_at(c("averageweight",
                    "playingtime",
                    "minplaytime",
                    "maxplaytime",
                    "yearpublished"),
                  ~ case_when(. == 0 ~ NA_real_,
                              TRUE ~ .)) %>%
        arrange(desc(bayesaverage))

# get links
game_links<- bq_table_download(bq_project_query(PROJECT_ID,
                                                'SELECT * FROM bgg.api_game_links'))

# get player counts
game_playercounts = 
        bq_table_download(bq_project_query(PROJECT_ID,
                                           'SELECT * FROM bgg.api_game_playercounts'))

# create additional tables ------------------------------------------------

message("creating additional tables..")

# games that have not been released
unreleased_games = 
        game_links %>%
        filter(value == 'Admin: Unreleased Games') %>%
        transmute(
                game_id, 
                value,
                family_id = id,
                family = type,
                load_ts
        )

# games with description issues/flags/issues
problem_games = game_links %>%
        # anything flagged with Admin:
        filter(grepl("Admin:", value)) %>%
        # remove upcoming releases
        filter(value != 'Admin: Unreleased Games') %>%
        transmute(
                game_id, 
                value,
                family_id = id,
                family = type,
                load_ts
        )

# game compilations aka big boxes
game_compilations =
        game_links %>%
        filter(type == 'compilation') %>%
        transmute(
                type,
                compilation_name = value,
                compilation_game_id = id,
                game_id,
                load_ts)

# game reimplementations and editions
game_implementations =
        game_links %>%
        filter(type == 'implementation') %>%
        transmute(
                type,
                implementation_name = value,
                implementation_game_id = id,
                game_id,
                load_ts)

# games by bgg categories
game_categories =
        game_links %>%
        filter(type == 'category') %>%
        transmute(type,
                  category_value = value,
                  category_id = id,
                  game_id,
                  load_ts)

# games by bgg family categories
game_families =
        game_links %>%
        filter(type == 'family') %>%
        separate(value,
                 into = c("family_type", "family_value"),
                 sep= ": ",
                 extra = "merge",
                 fill = "right") %>%
        transmute(type,
                  family_type,
                  family_value,
                  family_id = id,
                  game_id,
                  load_ts)

# game designers
game_designers = 
        game_links %>%
        filter(type == 'designer') %>%
        transmute(type,
                  designer_value = value,
                  designer_id = id,
                  game_id,
                  load_ts)

# game publishers
game_publishers = 
        game_links %>%
        filter(type == 'publisher') %>%
        transmute(type,
                  publisher_value = value,
                  publisher_id = id,
                  game_id,
                  load_ts)

message("done.")

# # remove unreleased and problem games
# tidy_games = 
#         analysis_games %>%
#         # remove unreleased and problem games
#         filter(!(game_id %in% c(
#                 unreleased_games$game_id,
#                 problem_games$game_id))
#         )



