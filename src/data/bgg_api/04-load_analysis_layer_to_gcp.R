# who: phil henrickson
# what: query gcp tables to get datasets of games for analysis
# when: 12/20/22

message("querying tables from gcp for analysis...")

#  packages ---------------------------------------------------------------

suppressPackageStartupMessages(
        {
                require(tidyverse)
                require(tidymodels)
        }
)

# resolve conflict preferences
tidymodels_prefer()


# connect to gcp ----------------------------------------------------------

source(here::here("src", "helpers", "connect_to_gcp.R"))


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

# get descriptions
game_descriptions = 
        bq_table_download(bq_project_query(PROJECT_ID,
                                           'SELECT * FROM bgg.api_game_descriptions'))

# get images
game_images = 
        bq_table_download(bq_project_query(PROJECT_ID,
                                           'SELECT * FROM bgg.api_game_images'))

# create additional tables ------------------------------------------------

message("creating additional tables..")

# games that have not been released
unreleased_games = 
        game_links %>%
        filter(value == 'Admin: Unreleased Games' | value == 'Upcoming Releases') %>%
        transmute(
                type,
                value,
                id,
                game_id,
                load_ts
        )

# games to drop from from _all_ analysis
# games that are expansions for base games/fan expansions
# games flagged with admin (excluding unreleased games)
# games that are missing on yearpublished
drop_games = 
        # add in yearpublished and numweights for filtering
        game_links %>%
        left_join(.,
                  analysis_games %>%
                          select(game_id, name, yearpublished) %>%
                          distinct,
                  by = c("game_id")) %>%
        # remove upcoming releases from this set
        filter(value != 'Admin: Unreleased Games') %>%
        # select games if they meet any of the following criteria
        filter(
                # expansion for base game or looking for a publisher
                value %in% 
                        c('Expansion for Base-game',
                          'Fan Expansion',
                          '(Looking for a publisher)') |
                        # # games where there's an admin note, as this usually indicates a data quality problem
                        grepl("Admin: Book entries that should be split", value) |
                        grepl("Admin: Cancelled Games", value) |
                        grepl("Admin: Miscellaneous Placeholder", value) |
                        grepl("Admin: Outside the Scope of BGG", value) |
                        grepl("Admin: Test Family for revision", value) |
                    #    missingness on yearpublished
                        is.na(yearpublished) |
                        is.na(name)
        ) %>%
        select(game_id, name, load_ts) %>%
        unique

# game compilations aka big boxes
game_compilations =
        game_links %>%
        filter(type == 'compilation') %>%
        transmute(
                type,
                name = value,
                id = id,
                game_id,
                load_ts)

# game reimplementations and editions
game_implementations =
        game_links %>%
        filter(type == 'implementation') %>%
        transmute(
                type,
                name = value,
                id = id,
                game_id,
                load_ts)

# games by bgg categories
game_categories =
        game_links %>%
        filter(type == 'category') %>%
        transmute(type,
                  value = value,
                  id = id,
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
                  value = paste(family_type, family_value),
                  id,
                  game_id,
                  load_ts)

# game designers
game_designers = 
        game_links %>%
        filter(type == 'designer') %>%
        transmute(type,
                  value,
                  id = id,
                  game_id,
                  load_ts)

# game publishers
game_publishers = 
        game_links %>%
        filter(type == 'publisher') %>%
        transmute(type,
                  value,
                  id = id,
                  game_id,
                  load_ts)

# game mechanics
game_mechanics = 
        game_links %>%
        filter(type == 'mechanic') %>%
        transmute(type,
                  value,
                  id = id,
                  game_id,
                  load_ts)

# game artists
game_artists = 
        game_links %>%
        filter(type == 'artist') %>%
        transmute(type,
                  value,
                  id = id,
                  game_id,
                  load_ts)

# load to analysis layer
# unreleased games
dbWriteTable(bigquerycon,
             name = "analysis_unreleased_games",
             append = T,
             value = unreleased_games)

# drop games
dbWriteTable(bigquerycon,
             name = "analysis_drop_games",
             append = T,
             value = drop_games)

# descriptions
dbWriteTable(bigquerycon,
             name = "analysis_game_descriptions",
             append = T,
             value = game_descriptions)

# images
dbWriteTable(bigquerycon,
             name = "analysis_game_images",
             append = T,
             value = game_images)

# categories
dbWriteTable(bigquerycon,
             name = "analysis_game_categories",
             append = T,
             value = game_categories)

# compilations
dbWriteTable(bigquerycon,
             name = "analysis_game_compilations",
             append = T,
             value = game_compilations)

# designers
dbWriteTable(bigquerycon,
             name = "analysis_game_designers",
             append = T,
             value = game_designers)

# families
dbWriteTable(bigquerycon,
             name = "analysis_game_families",
             append = T,
             value = game_families)

# implementations
dbWriteTable(bigquerycon,
             name = "analysis_game_implementations",
             append = T,
             value = game_implementations)

# publishers
dbWriteTable(bigquerycon,
             name = "analysis_game_publishers",
             append = T,
             value = game_publishers)

# artists
dbWriteTable(bigquerycon,
             name = "analysis_game_artists",
             append = T,
             value = game_artists)

# mechanics
dbWriteTable(bigquerycon,
             name = "analysis_game_mechanics",
             append = T,
             value = game_mechanics)


# save a copy of these tables for local use as an .Rdata object
message("saving a local copy...")
save(analysis_games,
     unreleased_games,
     drop_games,
     game_links,
     game_descriptions,
     game_images,
     game_categories,
     game_compilations,
     game_designers,
     game_families,
     game_implementations,
     game_playercounts,
     game_publishers,
     game_artists,
     game_mechanics,
     file = here::here("data", "local", "analysis_games_tables.Rdata"))

message("done.")




