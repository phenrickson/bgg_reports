# what: create local data sources to feed shiny apps

#############################################
### load scripts and functions ####
print("loading scripts and functions...")

suppressMessages({
source(here::here("scripts/load_packages.R"))
source(here::here("functions/tidy_name_func.R"))
source(here::here("functions/get_bgg_data_from_github.R"))
})


##############################################
### load games from beefsack's bgg table ###
print("loading daily games file from historical-bgg on github...")

bgg_today = get_bgg_data_from_github(Sys.Date())

# get ids for most recent pull from his table, this is a curated list of games
bgg_ids = bgg_today %>% 
        pull(game_id)

#####################################################
### set up connection to big query and get tables ###

print("connecting to big query...")

# connect to biquery
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

print("loading tables from big query...")

# query table of game info to most recent load
game_info<-DBI::dbGetQuery(bigquerycon, 
                           'SELECT * FROM bgg.api_game_info
                              where timestamp = (SELECT MAX(timestamp) as most_recent FROM bgg.api_game_info)') %>%
        mutate(numweights = as.numeric(numweights)) %>%
        mutate_at(c("averageweight",
                    "playingtime",
                    "minplaytime",
                    "maxplaytime",
                    "yearpublished"),
                  ~ case_when(. == 0 ~ NA_real_,
                              TRUE ~ .)) %>%
        filter(game_id %in% bgg_ids)

# long table with game type variables
game_types= DBI::dbGetQuery(bigquerycon, 
                            'SELECT * FROM bgg.api_game_categories') %>%
        filter(game_id %in% bgg_ids)

# recommended player counts
games_playercounts= DBI::dbGetQuery(bigquerycon, 
                                    'SELECT * FROM bgg.api_game_playercounts') %>%
        filter(!is.na(numplayers)) %>%
        mutate(numberplayers = as.numeric(gsub("\\+", "", numplayers))) %>%
        filter(numvotes > 0) %>%
        mutate(playercount = case_when(numberplayers > 8 ~ "8+",
                                       TRUE ~ as.character(numberplayers))) %>%
        filter(game_id %in% bgg_ids) %>%
        group_by(game_id, playercount) %>% 
        slice_max(order_by = numvotes, n = 1, with_ties=F) %>%
        ungroup()

# descriptions
game_descriptions = DBI::dbGetQuery(bigquerycon, 
                                    'SELECT * FROM bgg.api_game_descriptions') %>%
        filter(game_id %in% bgg_ids)

############################################################
#### load additional data pieces for estimating complexity ###
print("estimating complexity for games without user ratings...")

# load recipe
bgg_outcomes_recipe = 
        readr::read_rds(here::here("models", "active", "base_recipe.Rdata"))

# load workflow
bgg_outcomes_final_workflows = 
        readr::read_rds(here::here("models", "active", "bgg_outcomes_final_workflows.Rds"))

# categorical features used in model
categorical_features_selected = 
        readr::read_rds(here::here("data","categorical_features_selected.Rdata"))

# assemble data for modeling
games_model = game_info %>%
        left_join(.,
                  game_types %>%
                          left_join(., categorical_features_selected %>%
                                            select(type, id, value, tidied, selected),
                                    by = c("type", "id", "value")) %>%
                          filter(selected == 'yes') %>%
                          select(game_id, type, value) %>%
                          mutate(type_abbrev = substr(type, 1, 3)) %>%
                          mutate(value = tolower(gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", value))))) %>%
                          mutate(type = paste(type, value, sep="_")) %>%
                          mutate(has_type = 1) %>%
                          select(-value) %>%
                          pivot_wider(names_from = c("type"),
                                      values_from = c("has_type"),
                                      id_cols = c("game_id"),
                                      names_sep = "_",
                                      values_fn = min,
                                      values_fill = 0),
                  by = "game_id") %>%
        rename(numowned = owned) %>%
        rename(owned = numowned) %>%
        mutate(dataset = 'upcoming') %>%
        mutate(log_usersrated = log1p(usersrated)) %>%
        filter(!is.na(yearpublished))

# now estimate the averageweight for games that are missing averageweight
games_estimated_averageweight = bgg_outcomes_final_workflows %>%
        as_tibble() %>%
        filter(outcome == 'averageweight') %>%
        filter(grepl("xgbTree", wflow_id)) %>%
        mutate(preds = map(.workflow,
                           ~ .x %>%
                                   predict(new_data = games_model) %>%
                                   bind_cols(., games_model %>%
                                                     select(game_id)))) %>%
        select(preds) %>%
        unnest(preds) %>%
        rename(est_averageweight = .pred)

game_info %>%
        left_join(., games_estimated_averageweight,
                  by = c("game_id")) %>%
        ggplot(., aes(x=est_averageweight,
                      y=averageweight))+
        geom_point(alpha = 0.5)+
        theme_minimal()


###############################################################
#### now estimate complexity adjusted ratings ##### 

print("computing complexity adjusted ratings...")

# load workflow
complexity_adjusted_model = 
        readr::read_rds(here::here("models", "active", "complexity_model.Rds"))

# compute complexity adjusted rating
games_complexity_adjusted = complexity_adjusted_model %>%
        mutate(preds = map(model,
                           ~ .x %>% predict(games_model))) %>%
        select(preds) %>%
        unnest(preds) %>%
        bind_cols(., games_model %>%
                          select(game_id, usersrated)) %>%
        rename(adj_average = preds) %>%
        mutate(adj_bayesaverage = ((usersrated*adj_average) + (5.5*1800)) / (usersrated + 1800)) %>%
        arrange(desc(adj_bayesaverage)) %>%
        select(adj_average, adj_bayesaverage, game_id)

####################################################
##### now create data sources for dashboards ######

print("creating local data sources for dashboard...")
# now join
games_dashboard = game_info %>%
        left_join(., games_estimated_averageweight,
                  by = c("game_id")) %>%
        left_join(., games_complexity_adjusted,
                  by = c("game_id")) %>%
        rename(numowned = owned)

# game types
game_types_filtered = game_types %>%
        filter(type == 'category' |
                       (type == 'family' & grepl("Category:", value)) |
                       (type == 'family' & grepl("Mechanism|Components:", value)) |
                       type == 'designer' |
                       type == 'publisher' |
                       type == 'mechanic')

# save these locally for the dashboard
readr::write_rds(games_dashboard,
                 file = here::here("dashboards", "data", "games_dashboard.Rdata"))

readr::write_rds(game_types_filtered,
                 file = here::here("dashboards","data", "game_types_filtered.Rdata"))

readr::write_rds(games_playercounts,
                 file = here::here("dashboards", "data", "games_playercounts.Rdata"))

print("done.")

rm(list=ls())

gc()