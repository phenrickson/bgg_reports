# run bgg outcomes

# set params for run
end_train_year = 2019
min_ratings = 0

# run preprocessing
source(here::here("src", "models", "bgg_outcomes", "preprocessing.R"), echo=FALSE)

# create recipes and model specs
source(here::here("src", "models", "bgg_outcomes", "recipes_and_models.R"), echo=FALSE)

# train via workflow sets
source(here::here("src", "models", "bgg_outcomes", "training_models.R"), echo=FALSE)

# imputation
# source(here::here("src", "models", "bgg_outcomes", "imputation.R"), echo=TRUE)

# training