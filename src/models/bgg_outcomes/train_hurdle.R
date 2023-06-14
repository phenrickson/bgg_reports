# what: train a hurdle model to predict whether a game will receive a geek rating 
# aka achieve a user threshold rating

# dependencies:
# load analysis tables (locally) or run query


# packages ----------------------------------------------------------------

# core packages needed for modeling pipeline
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


# data --------------------------------------------------------------------

# load previously nested games dataset
load(here::here("data", "local", "games_nested.Rdata"))
