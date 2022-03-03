# run user notebook
library(tidyverse)
library(foreach)
library(bggAnalytics)

user_list = c("mrbananagrabber",
              "GOBbluth89",
              "ogzz",
              "Quinns",
              "Gyges",
              "rahdo",
              "TomVasel",
              "ZeeBarcia")

user_list = c("Gyges",
              "ZeeGarcia")

#              "ZeeGarcia")
              
year_end = 2020
ratings = 100

user_list = gsub(" ", "%20", user_list)

# run
foreach(i=1:length(user_list),
        .errorhandling = 'pass') %do% {
        rmarkdown::render(here::here("experiments/predict_user_collection.Rmd"), 
                          params = list(username = user_list[i],
                                        end_training_year = year_end,
                                        min_training_ratings = ratings),
                          output_file =  paste(
                                  gsub("%20", 
                                       "_",
                                       user_list[i]),
                                  year_end,
                                  sep = "_"),
                          output_dir = here::here("experiments/user_reports"))
}

rm(list=ls())
gc()
