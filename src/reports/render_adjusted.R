# what: render exploratory markdown reports

# set parameters
render_params = list(end_train_year = 2021,
                     min_ratings = 30)

# outcomes
rmarkdown::render(input = here::here("notebooks", "adjusting_bgg_ratings.Rmd"),
                  params = render_params,
                  clean = T,
                  output_dir = here::here("reports"))

rm(list=setdiff(ls(), c("render_params")))
gc()
