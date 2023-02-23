# run bgg outcomes

# set params for run
end_train_year = 2019
min_ratings = 0

# run preprocessing
source(here::here("src", "models", "bgg_outcomes", "preprocessing.R"), echo=FALSE)

# create recipes and model specs
source(here::here("src", "models", "bgg_outcomes", "recipes_and_models.R"), echo=FALSE)

# train model for imputing averageweight
source(here::here("src", "models", "bgg_outcomes", "impute_averageweight.R"), echo=FALSE)

# train outcomes: average, usersrated, bayesaverage
source(here::here("src", "models", "bgg_outcomes", "training_outcomes.R"), echo=FALSE)

# assess on validation set
source(here::here("src", "models", "bgg_outcomes", "assess_on_valid.R"), echo=FALSE)

# finalize and deploy
source(here::here("src", "models", "bgg_outcomes", "finalize_models.R"), echo=FALSE)

# imputation
# source(here::here("src", "models", "bgg_outcomes", "imputation.R"), echo=TRUE)

# training