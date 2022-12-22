## run game comparable report
library(tidyverse)
library(foreach)

# 
# ############################################
# ## pieces run ahead of time
# source(here::here("scripts", "load_packages.R"))
# library(httr)
# library(xml2)
# library(XML)
# library(rvest)
# library(purrr)
# 
# # cosine distance
# library(lsa)
# library(REdaS)
# 
# ## functions
# # core functions
# source(here::here("functions/get_bgg_data_from_api.R"))
# source(here::here("functions", "theme_phil.R"))
# source(here::here("functions", "tidy_name_func.R"))
# source(here::here("functions", "get_bgg_data_from_github.R"))
# source(here::here("functions", "n_min_func.R"))
# source(here::here("functions", "n_max_func.R"))
# source(here::here("functions", "find_neighbors_max_func.R"))
# source(here::here("functions", "find_neighbors_min_func.R"))
# source(here::here("functions", "dist_cosine_func.R"))
# source(here::here("functions", "dist_euclidean_func.R"))
# source(here::here("functions", "game_shap_func.R"))
# 
# # aesthetic functions
# source(here::here("functions", "tidy_name_func.R"))
# source(here::here("functions", "bayesaverage_col_func.R"))
# source(here::here("functions", "average_col_func.R"))
# source(here::here("functions", "complexity_col_func.R"))
# source(here::here("functions", "usersrated_col_func.R"))
# 
# # recreate some of these specifically for the report cards
# score_col_func = function(x) {
#         
#         breaks = c(4,
#                      4.5,
#                      5,
#                      5.25,
#                      5.5,
#                      5.75,
#                      6,
#                      6.25,
#                      6.5,
#                      6.75,
#                      7,
#                      7.25,
#                      7.5,
#                      8,
#                      8.5
#                      )
#         colorRamp=colorRampPalette(c("white", "deepskyblue1"))
#         col_palette <- colorRamp(length(breaks))
#         mycut <- cut(x, 
#                      breaks = breaks,
#                      include.lowest = TRUE, 
#                      right=T,
#                      label = FALSE)
#         col_palette[mycut]
#         
# }
# 
# usersrated_col_func <-
#         function(x) {
#                 breaks = c(0,
#                            100,
#                            500,
#                            1000,
#                            1500,
#                            2000,
#                            5000,
#                            10000,
#                            25000,
#                            50000,
#                            100000,
#                            1000000)
#                 
#                 colorRamp=colorRampPalette(c("white", "deepskyblue1"))
#                 col_palette <- colorRamp(length(breaks))
#                 mycut <- cut(x, 
#                              breaks = breaks,
#                              include.lowest = TRUE, 
#                              right=F,
#                              label = FALSE)
#                 col_palette[mycut]
#                 
#         }
# 
# 
# similarity_col_func <-
#         function(x) {
#                 breaks = seq(0.6, 1, 0.02)
#                 
#                 colorRamp=colorRampPalette(c("white", "deepskyblue1"))
#                 col_palette <- colorRamp(length(breaks))
#                 mycut <- cut(x, 
#                              breaks = breaks,
#                              include.lowest = TRUE, 
#                              right=F,
#                              label = FALSE)
#                 col_palette[mycut]
#                 
#         }
# ###################################################
# ## trained models and recipes for bgg outcomes #########
# bgg_outcomes_recipe = readr::read_rds(here::here("models", "active", "base_recipe.Rdata"))
# 
# # load workflow
# bgg_outcomes_final_workflows = readr::read_rds(here::here("models", "active", "bgg_outcomes_final_workflows.Rds"))
# 
# # extract template
# games_template = bgg_outcomes_recipe$template
# 
# ############################################
# ### pca recipes and number of pcs used for distance ####
# pca_recipe = readr::read_rds(here::here("models", "active", "pca_recipe.Rds"))
# pca_trained = readr::read_rds(here::here("models", "active", "pca_trained.Rds"))
# number_pcs = paste("PC", seq(1, 25), sep="")
# 
# # get template
# games_template = bgg_outcomes_recipe$template
# 
# ########################################
# ### some plots for the report card #####
# # tile plot func
# tile_plot = function(neighbors_cosine,
#                      input_pcs,
#                      input_game_id) {
#         
#         # create vector
#         number_pcs = paste("PC", seq(1, input_pcs), sep="")
#         
#         # make initial plot
#         dat =  neighbors_cosine %>%
#                 filter(game_id == input_game_id) %>%
#                 #       arrange(desc(score)) %>%
#                 mutate(neighbor_name = abbreviate(neighbor_name, 60)) %>%
#                 group_by(neighbor_name) %>% mutate(count = n()) %>%
#                 ungroup() %>%
#                 mutate(neighbor_name = case_when(count > 1 ~ paste(neighbor_name, neighbor_yearpublished, sep="_"),
#                                                  TRUE ~ neighbor_name))
#         
#         #return(dat)
#         
#         dat %>%
#                 select(neighbor_id, neighbor_name,
#                        all_of(number_pcs)) %>%
#                 gather("component", "value",
#                        -neighbor_id, -neighbor_name) %>%
#                 mutate(neighbor_name = factor(neighbor_name,
#                                               levels = rev(dat %>% select(neighbor_name) %>% pull()))) %>%
#                 mutate(component = factor(component,
#                                           levels = number_pcs)) %>%
#                 mutate(highlight = case_when(neighbor_id == input_game_id ~ 'yes',
#                                              TRUE ~ 'no')) %>%
#                 ggplot(., aes(y=neighbor_name,
#                               #   color = highlight,
#                               label = round(value, 2),
#                               fill = value,
#                               x=component))+
#                 geom_tile()+
#                 geom_text(color = 'white', size=2)+
#                 # scale_fill_viridis(limits = c(-8, 8),
#                 #                    oob = scales::squish)+
#                 # scale_fill_viridis(option = "A",
#                 #                    limits = c(-8,8),
#                 #                    oob = scales::squish)+
#                 scale_fill_gradient2(low = "orange",
#                                      mid = "grey80",
#                                      high = "navy",
#                                      limits = c(-6, 6),
#                                      oob = scales::squish)+
#                 # scale_fill_viridis(option = "B",
#                 #                    scale = )+
#                 theme_phil()+
#                 theme(legend.title = element_text()) +
#                 guides(fill = guide_colorbar(barwidth=10,
#                                              barheight=0.5,
#                                              title = "Component Score",
#                                              title.position = 'top'),
#                        color = "none")+
#                 scale_color_manual(values = c("white", "black"))
#         
# }
# 
# # make component loading plot
# plot_components = pca_trained %>%
#         tidy(id = "pca") %>%
#         mutate(component = gsub("PC0", "PC", gsub("PC00", "PC", component))) %>%
#         filter(component %in% number_pcs) %>%
#         mutate(component = factor(component,
#                                   levels = number_pcs)) %>%
#         group_by(component) %>%
#         slice_max(.,
#                   order_by = abs(value),
#                   n = 12,
#                   with_ties = F) %>%
#         mutate(terms = tidy_name_func(terms)) %>%
#         ggplot(., aes(x=value,
#                       fill = value,
#                       y = reorder_within(terms, value, component)))+
#         geom_col()+
#         facet_wrap(component~.,
#                    scales = "free_y",
#                    ncol = 4)+
#         theme_bw(8)+
#         scale_y_reordered()+
#       #  scale_fill_viridis()+
#         guides(fill = "none")+
#         ylab("")+
#         scale_fill_gradient2(low = "orange",
#                              mid = "grey80",
#                              high = "navy",
#                              oob = scales::squish)
# 
# ############################################
# # get games predicted list
# preds = readr::read_rds(here::here("predictions", "bgg_outcomes_2022-05-01.Rdata"))
# 
# # get games in report files
# reports_run = list.files(here::here("game_report_cards")) %>%
#         as_tibble() %>%
#         separate(value, into = c("game_id","file"), sep="\\.") %>%
#         mutate(game_id = as.numeric(game_id)) %>%
#         filter(!is.na(game_id)) %>%
#         mutate(report = 'yes')
# 
# # get list not run
# ids = preds %>%
#         arrange(desc(bayesaverage_indirect_xgbTree)) %>%
#         select(game_id, name, yearpublished) %>%
#         left_join(., reports_run,
#                   by = c("game_id")) %>%
#         filter(is.na(report)) %>%
#         filter(yearpublished > 2021) %>%
#         head(200)
# 
# # ###############################################
# # ### get ids from github #######
# # bgg_today = get_bgg_data_from_github(Sys.Date())
# # 
# # 
# # bgg_ids = bgg_today %>%
# #         filter(game_release_year %in% c(2021, 2022, 2023)) %>%
# #         arrange(desc(bayes_average)) %>%
# #         pull(game_id)
# # 
# # ids = c(259607,
# #         304324,
# #         321277,
# #         330555,
# #         339905,
# #         341918,
# #         344697,
# #         346143)
# #         

###################################

ids = c(260524,
        302892,
        317511,
        273910)
        
# run through
foreach(i=1:length(ids),
        .errorhandling = 'pass') %do% {
        rmarkdown::render(here::here("game_report_cards.Rmd"),
                          params = list(game_id = ids[i]),
                          output_file = ids[i],
                          output_dir = here::here("game_report_cards"))
}
