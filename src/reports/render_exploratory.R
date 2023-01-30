# what: render exploratory markdown reports

# set parameters
render_params = list(train_year = 2022,
              min_ratings = 25)

# outcomes
rmarkdown::render(input = here::here("notebooks", "outcomes.Rmd"),
                  params = render_params,
                  clean = T,
                  output_dir = here::here("reports", "exploratory"))

# descriptions
# rmarkdown::render(input = here::here("notebooks", "descriptions.Rmd"),
#                   params = render_params,
#                   output_dir = here::here("reports", "exploratory"))

# designers
rmarkdown::render(input = here::here("notebooks", "designers.Rmd"),
                  params = render_params,
                  clean =T,
                  output_dir = here::here("reports", "exploratory"))

# publishers
# rmarkdown::render(input = here::here("notebooks", "publishers.Rmd"),
#                   params = render_params,
#                   clean =T,
#                   output_dir = here::here("reports", "exploratory"))

# categories
rmarkdown::render(input = here::here("notebooks", "categories.Rmd"),
                  params = render_params,
                  clean =T,
                  output_dir = here::here("reports", "exploratory"))

# families
rmarkdown::render(input = here::here("notebooks", "families.Rmd"),
                  params = render_params,
                  clean = T,
                  output_dir = here::here("reports", "exploratory"))

# mechanics
rmarkdown::render(input = here::here("notebooks", "mechanics.Rmd"),
                  params = render_params,
                  clean = T,
                  output_dir = here::here("reports", "exploratory"))


rm(list=ls())
gc()

# done