# run user notebook
# user_list = c("legendarydromedary",
#               "lmageezy",
#               "C3Gaming",
#               "innerkudzu",
#               "Karmatic",
#               "camerinjohnston")
#user_list = c("jyothi")
# user_list = c("Booned",
#               "QWERTYMartin")
# user_list = c("Aeszett")

library(tidyverse)
library(foreach)

user_list = c("Watch%20It%20Played")

# user_list = c("rahdo",
#               "Quinns",
#             #  "ZeeGarcia",
#             "mrbananagrabber",
#               "Gyges", 
#               "GOBBluth89",
#       "Watch%20It%20Played")

# user_list = c("DTLibrary",
#               "Watch%20It%20Played",
#               "Quinns",
#               "rahdo",
#               "ZeeGarcia",
#               "Gyges",
#               "mrbananagrabber",
#               "GOBBluth89")

# user_list = c("
              # "ZeeGarcia",
              # "TomVasel",
              # "Gyges")

# run through
foreach(i=1:length(user_list)) %do% {
        rmarkdown::render(here::here("notebooks", "predicting_user_collections.Rmd"), 
                          params = list(username = user_list[i],
                                        end_training_year = 2020),
                          output_file =  paste(gsub("\\%20", "", user_list[i]), 2020, sep="_"),
                          output_dir = here::here("user_reports"))
}

rm(list=ls())
gc()
