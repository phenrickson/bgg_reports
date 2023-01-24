# what: render exploratory markdown reports

# set parameters
render_params = list(train_year = 2020,
              min_ratings = 50)

# outcomes
rmarkdown::render(input = here::here("notebooks", "outcomes.Rmd"),
                  params = render_params,
                  output_dir = here::here("reports", "exploratory"))

# mechanics
rmarkdown::render(input = here::here("notebooks", "mechanics.Rmd"),
                  params = render_params,
                  output_dir = here::here("reports", "exploratory"))


rm(list=ls())
gc()

# done