# run bgg outcomes

# set params for run
end_train_year = 2019
min_ratings = 25

# run preprocessing
source(here::here("src", "models", "bgg_outcomes", "preprocessing.R"), echo=TRUE)

# create recipes
source(here::here("src", "models", "bgg_outcomes", "recipes.R"), echo=TRUE)

# create model specs
source(here::here("src", "models", "bgg_outcomes", "models.R"), echo=TRUE)

# imputation
source(here::here("src", "models", "bgg_outcomes", "imputation.R"), echo=TRUE)

