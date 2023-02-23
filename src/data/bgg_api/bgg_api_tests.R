# load functions
# api
source(here::here("src", "data","bgg_api","bgg_api_functions.R"))

# tests
library(testthat)

### bgg api
# test one (older) game
ids = c(12)
one_game = get_bgg_games_data(ids,
                              tidy = T,
                              toJSON = F)

# test output of api for one game
test_that("one older game returned expected output from bgg api", {
        # list
        expect_success(expect_type(one_game, "list"))
        # one row of (nested) bgg games data (type, game_id, info, names, links, statistics, ranks, polls)
        expect_success(expect_equal(nrow(one_game$bgg_games_data), 1))
        # eight columns of data in row
        expect_success(expect_equal(ncol(one_game$bgg_games_data), 8))
})

# use transformation function to get tables
transform_one_game = transform_bgg_games_data(one_game)

# test transformation
test_that("one older game returned expected tables via transformation function", {
        # expect a length of 10
        expect_success(expect_length(transform_one_game, 10))
        
})

# pivot
rm(ids, one_game)

# test api for a newer game
ids = c(35784)
one_game = get_bgg_games_data(ids,
                              tidy = T,
                              toJSON = F)

# test for a newer game
test_that("one newer game returned expected output from bgg api", {
        # list
        expect_success(expect_type(one_game, "list"))
        # one row of (nested) bgg games data (type, game_id, info, names, links, statistics, ranks, polls)
        expect_success(expect_equal(nrow(one_game$bgg_games_data), 1))
        # eight columns of data in row
        expect_success(expect_equal(ncol(one_game$bgg_games_data), 8))
})

# test transformation
test_that("one newer game returned expected tables via transformation function", {
        # expect a length of 10
        expect_success(expect_length(transform_one_game, 10))
        
})

rm(ids, one_game)

# test function for multiple games
multiple_ids = c(12, 7, 169786)
multiple_games = get_bgg_games_data(multiple_ids,
                              tidy = T,
                              toJSON = F)

# test output of api for one game
test_that("multiple games returned expected output from bgg api", {
        # list
        expect_success(expect_type(multiple_games, "list"))
        # multiple rows of (nested) bgg games data (type, game_id, info, names, links, statistics, ranks, polls)
        expect_success(expect_equal(nrow(multiple_games$bgg_games_data), length(multiple_ids)-length(multiple_games$problem_game_ids)))
        # eight columns of data in row
        expect_success(expect_equal(ncol(multiple_games$bgg_games_data), 8))
})

# test transformations
transform_multiple_games = transform_bgg_games_data(multiple_games)

# test for a newer game
test_that("multiple games returned expected output via transformation function", {
        # expect a length of 10
        expect_success(expect_length(transform_multiple_games, 10))
        # expect row for each successful game
        expect_success(expect_equal(nrow(transform_multiple_games$game_info), length(multiple_ids)-length(multiple_games$problem_game_ids)))
})


