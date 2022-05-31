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

# game_info %>%
#         left_join(., games_estimated_averageweight,
#                   by = c("game_id")) %>%
#         ggplot(., aes(x=est_averageweight,
#                       y=averageweight))+
#         geom_point(alpha = 0.5)+
#         theme_minimal()


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

###############################################################
#### nearest neighbors 

print("now creating datasets with nearest neighbors...")

### pca recipes and number of pcs used for distance ####
pca_recipe = readr::read_rds(here::here("models", "active", "pca_recipe.Rds"))
pca_trained = readr::read_rds(here::here("models", "active", "pca_trained.Rds"))
number_pcs = paste("PC", seq(1, 20), sep="")

### functions
source(here::here("functions", "n_min_func.R"))
source(here::here("functions", "n_max_func.R"))
source(here::here("functions", "find_neighbors_max_func.R"))
source(here::here("functions", "find_neighbors_min_func.R"))
source(here::here("functions", "dist_cosine_func.R"))
source(here::here("functions", "dist_euclidean_func.R"))
source(here::here("functions", "game_shap_func.R"))

# save output
# replace and save
games_model_estimated = games_model %>%
        filter(!(game_id %in% (pca_recipe$template %>% pull(game_id)))) %>%
        left_join(., 
                  games_estimated_averageweight,
                  by = c("game_id")) %>%
        select(-averageweight) %>%
        rename(averageweight = est_averageweight)

# games to run through function, including original template + newly released games
pca_input = bind_rows(pca_recipe$template,
                      games_model %>%
                              filter(!(game_id %in% (pca_recipe$template %>% pull(game_id)))) %>%
                              left_join(., 
                                        games_estimated_averageweight,
                                        by = c("game_id")) %>%
                              select(-averageweight) %>%
                              rename(averageweight = est_averageweight))
                      
# apply pca to game, bind to pca
pca_out = pca_recipe %>%
        prep(strings_as_factor = F) %>%
        bake(new_data = pca_input) %>%
        set_names(., gsub("PC0", "PC", gsub("PC00", "PC", names(.))))

# get into matrix
pca_mat = pca_out %>%
        mutate(.row = row_number()) %>%
        select(.row, starts_with("PC")) %>%
        select(.row, all_of(number_pcs)) %>%
        column_to_rownames(".row") %>%
        as.matrix()

# get cosine similarity between all games
dist_cosine = dist_cosine_func(pca_mat)

# get neighbors from cosine
game_neighbors_cosine = find_neighbors_max_func(dist_cosine, 50) %>%
        left_join(., pca_out %>%
                          mutate(.row = row_number()) %>%
                          select(.row, game_id, name, average, bayesaverage),
                  by = c(".row")) %>%
        left_join(., pca_out %>%
                          mutate(.row_neighbor = row_number(),
                                 neighbor_usersrated = usersrated,
                                 neighbor_id = game_id,
                                 neighbor_name = name,
                                 neighbor_average = average,
                                 neighbor_yearpublished = yearpublished,
                                 neighbor_bayesaverage = bayesaverage) %>%
                          select(.row_neighbor, neighbor_id, neighbor_name, neighbor_average, neighbor_bayesaverage, neighbor_usersrated, neighbor_yearpublished, starts_with("PC")),
                  by = c(".row_neighbor")) %>%
        left_join(.,
                  pca_input %>%
                          select(game_id, averageweight) %>%
                          rename(neighbor_averageweight = averageweight,
                                 neighbor_id = game_id),
                  by = c("neighbor_id")) %>%
        rename(similarity = dist) %>%
        mutate(score = similarity*neighbor_bayesaverage)
                         

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


# games with features for modeling and estimated averageweight for upcoming games
games_model_estimated = games_model %>%
        filter(!(game_id %in% (pca_recipe$template %>% pull(game_id)))) %>%
        left_join(., 
                  games_estimated_averageweight,
                  by = c("game_id")) %>%
        select(-averageweight) %>%
        rename(averageweight = est_averageweight) %>%
        bind_rows(.,
                  games_model %>%
                          filter((game_id %in% (pca_recipe$template %>% pull(game_id)))))

# save these locally for the dashboard
readr::write_rds(games_dashboard,
                 file = here::here("dashboards", "data", "games_dashboard.Rdata"))

readr::write_rds(game_types_filtered,
                 file = here::here("dashboards","data", "game_types_filtered.Rdata"))

readr::write_rds(games_playercounts,
                 file = here::here("dashboards", "data", "games_playercounts.Rdata"))

readr::write_rds(game_neighbors_cosine,
                 file = here::here("dashboards", "data", "game_neighbors_cosine.Rdata"))

readr::write_rds(games_model_estimated,
                 file = here::here("dashboards", "data", "games_model_estimated.Rdata"))


print("done.")

rm(list=ls())

gc()