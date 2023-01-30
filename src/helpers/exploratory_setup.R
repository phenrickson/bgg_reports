# what: helper script for setup for Rmd exploratory reports

# knitr chunk options
knitr::opts_chunk$set(echo = F,
                      error=F,
                      warning = F,
                      dev="png",
                      fig.align = 'center',
                      out.width = '80%')

# options for displaying
options(knitr.duplicate.label = "allow",
        scipen = 999)

# packages
suppressPackageStartupMessages({
        
        # tidyverse/modeling
        library(tidyverse)
        library(tidymodels)
        library(tidyselect)
        
        # tidymodels preferences
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
        
        # tables
        library(gt)
        library(DT)
        
        # ggplot
        library(ggforce)
        
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


# functions
source(here::here("src", "helpers", "theme_phil.R"))

# # connect to gcp
# source(here::here("src", "helpers", "connect_to_gcp.R"))
# 
# # run script to pull tables from gcp
# source(here::here("src", "data", "pull_analysis_games_tables.R"))

# load local copy
load(here::here("data", "local", "analysis_games_tables.Rdata"))
