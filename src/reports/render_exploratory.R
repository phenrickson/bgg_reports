# what: render exploratory markdown reports

# set parameters
render_params = list(train_year = 2021,
              min_ratings = 30)

# outcomes
rmarkdown::render(input = here::here("notebooks", "outcomes.Rmd"),
                  params = render_params,
                  clean = T,
                  output_dir = here::here("reports", "exploratory"))

rm(list=setdiff(ls(), c("render_params")))
gc()

# descriptions
# rmarkdown::render(input = here::here("notebooks", "descriptions.Rmd"),
#                   params = render_params,
#                   output_dir = here::here("reports", "exploratory"))

# designers
rmarkdown::render(input = here::here("notebooks", "designers.Rmd"),
                  params = render_params,
                  clean =T,
                  output_dir = here::here("reports", "exploratory"))

rm(list=setdiff(ls(), c("render_params")))
gc()

# publishers
rmarkdown::render(input = here::here("notebooks", "publishers.Rmd"),
                  params = render_params,
                  clean =T,
                  output_dir = here::here("reports", "exploratory"))

rm(list=setdiff(ls(), c("render_params")))
gc()

# categories
rmarkdown::render(input = here::here("notebooks", "categories.Rmd"),
                  params = render_params,
                  clean =T,
                  output_dir = here::here("reports", "exploratory"))

rm(list=setdiff(ls(), c("render_params")))
gc()

# families
rmarkdown::render(input = here::here("notebooks", "families.Rmd"),
                  params = render_params,
                  clean = T,
                  output_dir = here::here("reports", "exploratory"))

rm(list=setdiff(ls(), c("render_params")))
gc()

# mechanics
rmarkdown::render(input = here::here("notebooks", "mechanics.Rmd"),
                  params = render_params,
                  clean = T,
                  output_dir = here::here("reports", "exploratory"))


rm(list=ls())
gc()

# done