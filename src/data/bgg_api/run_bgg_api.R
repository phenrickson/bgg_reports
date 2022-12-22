# who: phil henrickson
# what: run scripts to scrape bgg data and load to gcp
# when: 11/2/22

# 1 run scrape_bgg_ids.py
# scrapes pages of boardgamegeek to get univrse of ids
reticulate::source_python(here::here("scripts", "bgg_api", "00-scrape_bgg_ids.py"))

# 2 load bgg ids to gcp
# loads previously scraped file to gcp
source(here::here("scripts", "bgg_api", "01-load_bgg_ids_to_gcp.R"))

# 3 gets games from api
source(here::here("scripts", "bgg_api", "02-get_game_data_from_bgg_api.R"))

# 4 loads api game data to gcp
source(here::here("scripts", "bgg_api", "03-load_bgg_games_to_gcp.R"))
