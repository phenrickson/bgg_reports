library(tidyverse)
library(foreach)

#user_list = c("mrbananagrabber")
#user_list = c("Watch%20It%20Played")

# user_list = c("rahdo",
#               "Quinns",
#             #  "ZeeGarcia",
#             "mrbananagrabber",
#               "Gyges", 
#               "GOBBluth89",
#       "Watch%20It%20Played")

#user_list = c("DTLibrary")

user_list = c("mrbananagrabber",
              "Watch%20It%20Played",
              "Quinns",
              "rahdo",
              "ZeeGarcia",
              "Gyges",
              "mrbananagrabber",
              "GOBBluth89")

# user_list = c("
              # "ZeeGarcia",
              # "TomVasel",
              # "Gyges")

# run through
foreach(i=1:length(user_list)) %do% {
        
        rmarkdown::render(here::here("notebooks", "predicting_user_collections.Rmd"), 
                          params = list(username = user_list[i],
                                        end_training_year = 2020),
                          output_file =  paste(gsub("\\%20", "", user_list[i]), 2020, sep="_"))
#                          output_dir = here::here())
}

rm(list=ls())
gc()
