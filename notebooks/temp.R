source(here::here("functions", "theme_phil.R"))

library(plotly)
library(treemapify)
library(ggforce)
library(ggridges)


game_compilations = 
        game_links %>%
        filter(type == 'compilation') %>%
        transmute(
                type,
                compilation_name = value,
                compilation_game_id = id,
                game_id,
                load_ts)

game_implementations = 
        game_links %>%
        filter(type == 'implementation') %>%
        transmute(
                type,
                implementation_name = value,
                implementation_game_id = id,
                game_id,
                load_ts)

game_categories = 
        game_links %>%
        filter(type == 'category') %>%
        transmute(type,
                  category_value = value,
                  category_id = id,
                  game_id,
                  load_ts)

game_categories = 
        game_links %>%
        filter(type == 'category') %>%
        transmute(type,
                  category_value = value,
                  category_id = id,
                  game_id,
                  load_ts)
game_families = 
        game_links %>%
        filter(type == 'family') %>%
        separate(value, 
                 into = c("family_type", "family_value"), 
                 sep= ": ",
                 extra = "merge",
                 fill = "right") %>%
        transmute(type, 
                  family_type, 
                  family_value, 
                  family_id = id,
                  game_id,
                  load_ts)
                 
        transmute(type,
                  category_value = value,
                  category_id = id,
                  game_id,
                  load_ts)

game_categories %>%
        left_join(., analysis_games %>%
                          filter(usersrated > 100) %>%
                          select(game_id,
                                 name,
                                 bayesaverage,
                                 average),
                  by = c("game_id")) %>%
        # group_by(category_value, category_id) %>%
        # mutate(num_games = n_distinct(game_id)) %>%
        # filter(num_games > 100) %>%
        select(category_value, 
               category_id,
               game_id,
               name,
               bayesaverage,
               average) %>%
        group_by(category_value, category_id) %>%
        summarize(bayesaverage = mean(bayesaverage, na.rm=T),
                  average = mean(average, na.rm=T))


# plot 
game_categories %>%
        left_join(., analysis_games %>%
                          filter(usersrated > 100) %>%
                          select(game_id,
                                 name,
                                 bayesaverage,
                                 average),
                  by = c("game_id")) %>%
        group_by(category_value, category_id) %>%
        mutate(num_games = n_distinct(game_id)) %>%
        filter(num_games > 200) %>%
        select(category_value, 
               category_id,
               game_id,
               name,
               bayesaverage,
               average) %>%
        pivot_longer(cols = c("bayesaverage", "average"),
                     names_to = "outcome",
                     values_to = "value") %>%
        group_by(category_value, category_id, outcome) %>%
        mutate(category_median = median(value, na.rm=T)) %>%
        mutate(category_value = abbreviate(category_value, minlength = 25)) %>%
        ggplot(aes(x=value,
                   y = reorder_within(category_value,
                                      category_median,
                                      outcome)))+
        geom_boxplot(outlier.shape = NA)+
        facet_wrap(outcome ~.,
                   scales = "free")+
        #     stat_density_ridges(quantile_lines = T)+
        scale_y_reordered()+
        theme_phil()+
        ylab("BGG Category")+
        theme(axis.text.y = element_text(size = 7))+
        xlab("Value")

# user ratings and complexity
game_categories %>%
        left_join(., analysis_games %>%
                          filter(usersrated > 100) %>%
                          select(game_id,
                                 name,
                                 averageweight,
                                 usersrated),
                  by = c("game_id")) %>%
        mutate(usersrated = log(usersrated)) %>%
        group_by(category_value, category_id) %>%
        mutate(num_games = n_distinct(game_id)) %>%
        filter(num_games > 200) %>%
        select(category_value, 
               category_id,
               game_id,
               name,
               averageweight,
               usersrated) %>%
        pivot_longer(cols = c("averageweight", "usersrated"),
                     names_to = "outcome",
                     values_to = "value") %>%
        group_by(category_value, category_id, outcome) %>%
        mutate(category_median = median(value, na.rm=T)) %>%
        mutate(category_value = abbreviate(category_value, minlength = 25)) %>%
        ggplot(aes(x=value,
                   y = reorder_within(category_value,
                                      category_median,
                                      outcome)))+
        geom_boxplot(outlier.shape = NA)+
        facet_wrap(outcome ~.,
                   scales = "free")+
        #     stat_density_ridges(quantile_lines = T)+
        scale_y_reordered()+
        theme_phil()+
        ylab("BGG Category")+
        theme(axis.text.y = element_text(size = 7))+
        xlab("Value")

analysis_games %>%
        filter(yearpublished < 2022) %>%
        filter(usersrated > 100) %>%
        ggplot(aes(x=averageweight,
                   y=log(usersrated)))+
        geom_point(alpha = 0.25,
                   position = position_jitternormal(sd_x = 0.05))+
        theme_phil()+
        geom_smooth()

analysis_games %>%
        filter(yearpublished < 2022) %>%
        filter(usersrated > 100) %>%
        ggplot(aes(x=averageweight,
                   y=average))+
        geom_point(alpha = 0.25
                   position = position_jitternormal(sd_y = 0.05,
                                                    sd_x = 0.05))+
        theme_phil()

game_categories = 
        game_links %>%
        filter(type == 'category')


game_families %>%
        filter(family_value == 'Upcoming Releases') %>%
        left_join(., analysis_games %>%
                          select(name, game_id))
                          
# plot games by BGG family
(game_families %>%
                group_by(family_type) %>%
                count(sort=T) %>%
                #  group_by(grp = as.integer(gl(n(), 50, n()))) %>%
                ggplot(aes(x=n,
                           y=reorder(family_type, n),
                           # label = n,
                           # label2 = family_type,
                           text = paste(
                                   paste("Family:", family_type),
                                   paste("Games:", n),
                                   sep = "\n")))+
                geom_col()+
                theme_phil()+
                theme(axis.text.y = element_text(size = 6))+
                ylab("BGG Family")+
                xlab("Number of Games")) %>%
        ggplotly(tooltip = c("text"))

# plot games within main families
# plot family type within top families
game_families %>%
        filter(family_type %in% 
                       c("Players",
                         "Theme",
                         "Components",
                         "game")) %>%
        mutate(family_value = abbreviate(family_value, minlength=20)) %>%
        group_by(family_type, family_value) %>%
        count(sort=T) %>%
        group_by(family_type) %>%
        slice_max(order_by = n,
                  n = 40,
                  with_ties = F) %>%
        ggplot(aes(x=n,
                   y=reorder_within(family_value,n, family_type)))+
        geom_col()+
        facet_wrap(family_type ~.,
                   ncol = 2,
                   scales = "free")+
        scale_y_reordered()+
        theme_phil()


game_families %>%
        filter(family_type == 'Theme') %>%
        group_by(family_type, family_value) %>%
        count(sort=T) %>%
        ggplot(aes(area = n,
                   fill = n,
                   label = paste(family_value,
                                 n,
                                 sep = "\n"),
                   group = family_value))+
        geom_treemap(color = 'white')+
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 8)+
        guides(fill = "none")+
        theme_phil()





game_families %>%
        filter(family_type == 'Theme') %>%
        group_by(family_type, family_value) %>%
        count(sort=T)  %>%
        ungroup() %>%
        mutate(rank = row_number()) %>%
        mutate(family_value = case_when(rank > 50 ~ 'Other',
                                        TRUE ~ family_value))
        # group_by(grp = as.integer(gl(n(), 50, n()))) %>%
        # mutate(grp_name = case_when(grp == 1 ~ paste(grp, grp*50, sep="-"),
        #                             TRUE ~ paste((grp-1)*50, grp*50, sep="-"))) %>%
        # ungroup() %>%
        arrange(desc(n), family_value) %>%
        mutate(family_value = abbreviate(family_value, minlength=20)) %>%
        ggplot(aes(x=n,
                   reorder(family_value,n)))+
        geom_col()+
        theme_phil()+
        theme(axis.text.y = element_text(size = 8,hjust = 0),
              strip.text.x = element_blank())+
        facet_wrap_paginate(.~ grp_name,
                            ncol = 3,
                            scales = "free_y")+
        ylab("Theme")+
        xlab("Number of Games")

        
                       
                        
                        
                )
        game_families %>%
                group_by(family_type) %>%
                count(sort=T) %>%
                #  group_by(grp = as.integer(gl(n(), 50, n()))) %>%
                ggplot(aes(x=n,
                           y=reorder(family_type, n),
                           # label = n,
                           # label2 = family_type,
                           text = paste(
                                   paste("Family:", family_type),
                                   paste("Games:", n),
                                   sep = "\n")))+
                geom_col()+
                theme_phil()+
                theme(axis.text.y = element_text(size = 6))+
                ylab("BGG Family")+
                xlab("Number of Games")) %>%
        ggplotly(tooltip = c("text"))



# plot games by BGG family
(game_families %>%
                group_by(family_type) %>%
                count(sort=T) %>%
                #  group_by(grp = as.integer(gl(n(), 50, n()))) %>%
                ggplot(aes(x=n,
                           y=reorder(family_type, n),
                           # label = n,
                           # label2 = family_type,
                           text = paste(
                                   paste("Family:", family_type),
                                   paste("Games:", n),
                                   sep = "\n")))+
                geom_col(fill = 'grey80')+
                theme_phil()+
                theme(axis.text.y = element_text(size = 6))+
                ylab("BGG Family")+
                xlab("Number of Games")) %>%
        ggplotly(tooltip = c("text")) 
                config(displayModeBar = F)
                
        
game_implementations %>%
        filter(grepl("Rococo", implementation_name))


game_links %>%
        filter(type == 'mechanic') %>%
        mutate(value = 
                       str_to_lower(
                               gsub("[[:space:]]", "_", 
                                    gsub("\\s+", " ",
                                         gsub("[[:punct:]]", "", value))))
        ) %>%
        mutate(value = paste("mechanic_", value, sep="")) %>%
        mutate(has_feature = 1) %>%
        pivot_wider(names_from = c("value"),
                    values_from = c(has_feature),
                    id_cols = c("game_id")) %>%
        mutate_if(is.numeric, replace_na, 0)

analysis_games %>%
        filter(game_id %in% 
                       ()

game_links %>%
        filter(type == 'mechanic') %>%
        left_join(., analysis_games %>%
                          select(game_id, name, yearpublished, bayesaverage,



