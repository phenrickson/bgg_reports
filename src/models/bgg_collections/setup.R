# what: setup for user modeling

message('preprocessing games for user collection modeling:')

message('loading packages and functions...')


# packages ----------------------------------------------------------------


# load core packages needed for modeling
suppressPackageStartupMessages({
        
        # tidyverse/modeling
        library(tidyverse)
        library(tidymodels)
        library(tidyselect)
        
        # set tidymodels preferences
        tidymodels_prefer()
        
        # recipes and workflows
        library(recipes)
        library(workflows)
        library(workflowsets)
        
        # additional
        library(magrittr)
        library(broom.mixed)
        library(data.table)
        library(tidytext)
        library(conflicted)
        library(lubridate)
        
        # ggplot
        library(ggforce)
        library(ggthemes)
        library(ggforce)
        library(ggfortify)
        
        # pins
        library(pins)
        
        # vetiver
        library(vetiver)
        
        # tests
        library(testthat)
}
)

# conflicts
suppressMessages({
        conflict_prefer("year", "lubridate")
        conflict_prefer("quarter", "lubridate")
        conflict_prefer("set_names", "magrittr")
        conflict_prefer("flatten", "purrr")
        conflict_prefer("tune", "tune")
        conflict_prefer("plotly", "layout")
})


# functions ---------------------------------------------------------------


# bgg collection 
source(here::here("src", "data", "bgg_collections", "bgg_collection_functions.R"))

# bgg outcomes
source(here::here("src", "data", "bgg_outcomes", "bgg_outcomes_functions.R"))


# data --------------------------------------------------------------------

message('loading (local) data for games...')

# # bgg data
# # load tables used in modeling
# # local version
# load(here::here("data", "local", "analysis_games_tables.Rdata"))

# # query from gcp (and update local)
# source(here::here("pull_analysis_games_tables.R"))

# nested bgg data with all info
load(here::here("data", "local", "games_nested.Rdata"))

# publisher allow list
processed_board = board_folder(here::here("data", "processed"), versioned = T)

# read in publisher allow list
publisher_allow_list = processed_board %>% pin_read("publisher_allow_list")