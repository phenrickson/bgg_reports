
library(tidyverse)
library(ggthemes)
library(ggrepel)

source("functions/theme_phil.R")
games_model = readr::read_rds("/Users/phenrickson/Documents/projects/bgg/predicting_bgg_outcomes/data/games_model.Rdata")

options(scipen = 999)

dat =  games_model %>%
        filter(!is.na(yearpublished)) %>%
        filter(!is.na(average)) %>%
        filter(!is.na(usersrated)) %>%
        filter(bayesaverage != 0) %>%
        filter(usersrated > 1) %>%
        mutate(average_z_score = (average - mean(average) / sd(average)))

p1 = dat %>%
        ggplot(., aes(x=average,
                      label = name,
                      color = average,
                      y = usersrated))+
        geom_point(alpha = 0.3)+
        scale_y_log10()+
        geom_text(check_overlap = T,
                  vjust = -1,
                  size = 2)+
        scale_color_gradient2(high = "deepskyblue1",
                              low = "red",
                              mid = "grey60",
                              limits = c(4, 8),
                              oob = scales::squish,
                              midpoint = 6)+
        theme_phil()+
        theme(legend.position = 'top',
              legend.title = element_text())+
        theme(axis.text = element_text())+
        guides(color = guide_colorbar(barwidth = 12,
                                      barheight=0.35,
                                      title = 'low rating                     high rating',
                                      title.position = 'top'))+
        xlab("bgg average rating")+
        ylab("users rated (logged)")+
        labs(caption = paste("data from boardgamegeek.com",
                             paste("data last updated on", as.Date(games_model$timestamp[1])),
                             "analysis and code at github.com/phenrickson/bgg",
                             sep = "\n"))



p2 = dat %>%
        ggplot(., aes(x=average,
                      label = name,
                      color = average,
                      y = usersrated))+
        geom_point(alpha = 0.15)+
        scale_y_log10()+
        scale_color_gradient2(high = "deepskyblue1",
                              low = "red",
                              mid = "grey60",
                              limits = c(4, 8),
                              oob = scales::squish,
                              midpoint = 6)+
        theme_phil()+
        theme(legend.position = 'top',
              legend.title = element_text())+
        theme(axis.text = element_text())+
        guides(color = guide_colorbar(barwidth = 12,
                                      barheight=0.35,
                                      title = 'low rating                     high rating',
                                      title.position = 'top'))+
        xlab("bgg average rating")+
        ylab("users rated (logged)")+
        labs(caption = paste("data from boardgamegeek.com",
                             paste("data last updated on", as.Date(games_model$timestamp[1])),
                             "analysis and code at github.com/phenrickson/bgg",
                             sep = "\n"))+
        geom_point(data = dat %>%
                           arrange(desc(bayesaverage)) %>%
                           head(50),
                   aes(x=average,
                       color = average,
                       y = usersrated))+
        geom_text_repel(data = dat %>%
                           arrange(desc(bayesaverage)) %>%
                           head(50),
                   aes(x=average,
                       color = average,
                       y = usersrated),
                   max.overlaps = 20,
                  size = 2)

top_n = 100

p2 = dat %>%
        ggplot(., aes(x=average,
                      label = name,
                      color = average,
                      y = usersrated))+
        geom_point(alpha = 0.15)+
        scale_y_log10()+
        scale_color_gradient2(high = "deepskyblue1",
                              low = "red",
                              mid = "grey60",
                              limits = c(4, 8),
                              oob = scales::squish,
                              midpoint = 6)+
        theme_phil()+
        theme(legend.position = 'top',
              legend.title = element_text())+
        theme(axis.text = element_text())+
        guides(color = guide_colorbar(barwidth = 12,
                                      barheight=0.35,
                                      title = 'low rating                     high rating',
                                      title.position = 'top'))+
        xlab("bgg average rating")+
        ylab("users rated (logged)")+
        labs(caption = paste("data from boardgamegeek.com",
                             paste("data last updated on", as.Date(games_model$timestamp[1])),
                             "analysis and code at github.com/phenrickson/bgg",
                             sep = "\n"))+
        geom_point(data = dat %>%
                           arrange(desc(bayesaverage)) %>%
                           head(top_n),
                   aes(x=average,
                       color = average,
                       y = usersrated))+
        geom_text_repel(data = dat %>%
                                arrange(desc(bayesaverage)) %>%
                                head(top_n),
                        aes(x=average,
                            color = average,
                            y = usersrated),
                        max.overlaps = 20,
                        size = 2)

p3 = dat %>%
        ggplot(., aes(x=average,
                      label = name,
                      color = average,
                      y = usersrated))+
        geom_point(alpha = 0.15)+
        scale_y_log10()+
        scale_color_gradient2(high = "deepskyblue1",
                              low = "red",
                              mid = "grey60",
                              limits = c(4, 8),
                              oob = scales::squish,
                              midpoint = 6)+
        theme_phil()+
        theme(legend.position = 'top',
              legend.title = element_text())+
        theme(axis.text = element_text())+
        guides(color = guide_colorbar(barwidth = 12,
                                      barheight=0.35,
                                      title = 'low rating                     high rating',
                                      title.position = 'top'))+
        xlab("bgg average rating")+
        ylab("users rated (logged)")+
        labs(caption = paste("data from boardgamegeek.com",
                             paste("data last updated on", as.Date(games_model$timestamp[1])),
                             "analysis and code at github.com/phenrickson/bgg",
                             sep = "\n"))+
        geom_point(data = dat %>%
                           mutate(adj_bayesaverage = ((5.5*10000) + (usersrated*average)) / (10000 + usersrated)) %>%
                           arrange(desc(adj_bayesaverage)) %>%
                           head(top_n),
                   aes(x=average,
                       color = average,
                       y = usersrated))+
        geom_text_repel(data = dat %>%
                                mutate(adj_bayesaverage = ((5.5*10000) + (usersrated*average)) / (10000 + usersrated)) %>%
                                arrange(desc(adj_bayesaverage)) %>%
                                head(top_n),
                        aes(x=average,
                            color = average,
                            y = usersrated),
                        max.overlaps = 20,
                        size = 2)

p4 = dat %>%
        ggplot(., aes(x=bayesaverage,
                      label = name,
                      color = bayesaverage,
                      y = usersrated))+
        geom_point(alpha = 0.15)+
        geom_text(check_overlap = T,
                  vjust = -1,
                  size = 2)+
        scale_y_log10()+
        scale_color_gradient2(high = "deepskyblue1",
                              low = "red",
                              mid = "grey60",
                              limits = c(4, 8),
                              oob = scales::squish,
                              midpoint = 6)+
        theme_phil()+
        theme(legend.position = 'top',
              legend.title = element_text())+
        theme(axis.text = element_text())+
        guides(color = guide_colorbar(barwidth = 12,
                                      barheight=0.35,
                                      title = 'low rating                     high rating',
                                      title.position = 'top'))+
        xlab("bgg geek rating")+
        ylab("users rated (logged)")+
        labs(caption = paste("data from boardgamegeek.com",
                             paste("data last updated on", as.Date(games_model$timestamp[1])),
                             "analysis and code at github.com/phenrickson/bgg",
                             sep = "\n"))

        # geom_point(data = dat %>%
        #                    mutate(adj_bayesaverage = ((5.5*10000) + (usersrated*average)) / (10000 + usersrated)) %>%
        #                    arrange(desc(adj_bayesaverage)) %>%
        #                    head(top_n),
        #            aes(x=average,
        #                color = average,
        #                y = usersrated))+
        # geom_text_repel(data = dat %>%
        #                         mutate(adj_bayesaverage = ((5.5*10000) + (usersrated*average)) / (10000 + usersrated)) %>%
        #                         arrange(desc(adj_bayesaverage)) %>%
        #                         head(top_n),
        #                 aes(x=average,
        #                     color = average,
        #                     y = usersrated),
        #                 max.overlaps = 20,
        #                 size = 2)+
        #         coord_flip()
        # 
        
p5 = dat %>%
        mutate(adj_bayesaverage = ((5.5 * 10000) + (usersrated * average)) / (10000 + usersrated)) %>%
        ggplot(., aes(x=adj_bayesaverage,
                              label = name,
                              color = adj_bayesaverage,
                              y = usersrated))+
                geom_point(alpha = 0.15)+
                geom_text(check_overlap = T,
                          vjust = -1,
                          size = 2)+
                scale_y_log10()+
                scale_color_gradient2(high = "deepskyblue1",
                                      low = "red",
                                      mid = "grey60",
                                      limits = c(4, 8),
                                      oob = scales::squish,
                                      midpoint = 6)+
                theme_phil()+
                theme(legend.position = 'top',
                      legend.title = element_text())+
                theme(axis.text = element_text())+
                guides(color = guide_colorbar(barwidth = 12,
                                              barheight=0.35,
                                              title = 'low rating                     high rating',
                                              title.position = 'top'))+
                xlab("bgg geek rating")+
                ylab("users rated (logged)")+
                labs(caption = paste("data from boardgamegeek.com",
                                     paste("data last updated on", as.Date(games_model$timestamp[1])),
                                     "analysis and code at github.com/phenrickson/bgg",
                                     sep = "\n"))
        
        geom_point(data = dat %>%
                           mutate(adj_bayesaverage = ((5.5*10000) + (usersrated*average)) / (10000 + usersrated)) %>%
                           arrange(desc(adj_bayesaverage)) %>%
                           head(top_n),
                   aes(x=average,
                       color = average,
                       y = usersrated))+
                geom_text_repel(data = dat %>%
                                        mutate(adj_bayesaverage = ((5.5*10000) + (usersrated*average)) / (10000 + usersrated)) %>%
                                        arrange(desc(adj_bayesaverage)) %>%
                                        head(top_n),
                                aes(x=average,
                                    color = average,
                                    y = usersrated),
                                max.overlaps = 20,
                                size = 2)+
                coord_flip()
        


# fit linear model to get the residual
mod = dat %>%
        nest(data = everything()) %>%
        mutate(complexity_lm = map(data, ~ lm(average~averageweight,
                                              data = .x))) %>%
        mutate(resid = map(complexity_lm, ~ resid(.x))) %>%
        select(data, resid) %>%
        unnest() %>%
        select(game_id, name, yearpublished, average, bayesaverage, resid, usersrated)

mod %>%
        arrange(desc(resid)) %>%
        mutate(adj_average = resid + mean(average, na.rm=T)) %>%
        mutate(complexity_adjusted = ((usersrated*adj_average) + (5.5 * 1000))/ (1000 + usersrated)) %>%
        arrange(desc(complexity_adjusted)) %>%
        mutate(mean_average = mean(average)) %>%
        ggplot(., aes(x=complexity_adjusted,
                      label = name,
                      color = complexity_adjusted,
                      y = usersrated))+
        geom_text(check_overlap = T,
                  vjust = -1,
                  size = 2)+
        geom_point(alpha = 0.15)+
        scale_y_log10()+
        scale_color_gradient2(high = "deepskyblue1",
                              low = "red",
                              mid = "grey60",
                              limits = c(4, 8),
                              oob = scales::squish,
                              midpoint = 6)+
        theme_phil()+
        theme(legend.position = 'top',
              legend.title = element_text())+
        theme(axis.text = element_text())+
        guides(color = guide_colorbar(barwidth = 12,
                                      barheight=0.35,
                                      title = 'low rating                     high rating',
                                      title.position = 'top'))+
        xlab("bgg average rating")+
        ylab("users rated (logged)")+
        labs(caption = paste("data from boardgamegeek.com",
                             paste("data last updated on", as.Date(games_model$timestamp[1])),
                             "analysis and code at github.com/phenrickson/bgg",
                             sep = "\n"))

c1 = mod %>%
        arrange(desc(resid)) %>%
        mutate(adj_average = resid + mean(average, na.rm=T)) %>%
        mutate(complexity_adjusted = ((usersrated*adj_average) + (5.5 * 1000))/ (1000 + usersrated)) %>%
        arrange(desc(complexity_adjusted)) %>%
        mutate(mean_average = mean(average)) %>%
        ggplot(., aes(x=mean_average + resid,
                      label = name,
                      color = mean_average + resid,
                      y = usersrated))+
        geom_text(check_overlap = T,
                  vjust = -1,
                  size = 2)+
        geom_point(alpha = 0.15)+
        scale_y_log10()+
        scale_color_gradient2(high = "deepskyblue1",
                              low = "red",
                              mid = "grey60",
                              limits = c(4, 8),
                              oob = scales::squish,
                              midpoint = 6)+
        theme_phil()+
        theme(legend.position = 'top',
              legend.title = element_text())+
        theme(axis.text = element_text())+
        guides(color = guide_colorbar(barwidth = 12,
                                      barheight=0.35,
                                      title = 'low rating                     high rating',
                                      title.position = 'top'))+
        xlab("bgg average rating")+
        ylab("users rated (logged)")+
        labs(caption = paste("data from boardgamegeek.com",
                             paste("data last updated on", as.Date(games_model$timestamp[1])),
                             "analysis and code at github.com/phenrickson/bgg",
                             sep = "\n"))

        geom_point(data = dat %>%
                           arrange(desc(bayesaverage)) %>%
                           head(50),
                   aes(x=average,
                       color = average,
                       y = usersrated))+
        geom_text_repel(data = dat %>%
                                arrange(desc(bayesaverage)) %>%
                                head(50),
                        aes(x=average,
                            color = average,
                            y = usersrated),
                        max.overlaps = 20,
                        size = 2)

        
        
                   
                



        

        select(game_id, name, yearpublished, averageweight, average, usersrated) %>%
        gather("variable", "value",
               -yearpublished, -game_id, -name) %>%
        group_by(variable) %>%
        mutate(percentile = rank(value)/length(value)) %>%
        pivot_wider(id_cols = c("name", "game_id", "yearpublished"),
                    names_from = c("variable"),
                    values_from = c("value", "percentile")) %>%
        ggplot(., aes(x=percentile_average,
                      label = name,
                      y=percentile_usersrated))+
        geom_jitter(alpha = 0.25)+
        coord_cartesian(xlim = c(-0.1, 1.1),
                        ylim = c(-0.1, 1.1))+
                
        geom_text(check_overlap=T,
                  size = 1.5,
                  vjust = -1)


games_model %>%
        ggplot(., aes(x=average,
                      label = name,
                      y = usersrated))+
        geom_point(alpha = 0.5)+
        geom_text(check_overlap=T,size=3,vjust = -1)+
        theme_bw(8)+
        scale_y_log10()



games_model %>% 
        filter(family_animals_dinosaurs == 1) %>% 
        mutate(averageweight = case_when(averageweight == 0 ~ 1, TRUE ~ averageweight))%>%
        filter(yearpublished > 1980) %>% 
        ggplot(., aes(x=averageweight,
                      color = average,
                      size = usersrated,
                      label = name, y=average))+
        geom_jitter(alpha = 0.5)+
        geom_text_repel(size =3,
                        max.overlaps = 10)+
        theme_phil()+
        ggtitle("What are the highest rated and most complex dinosaur games?",
                subtitle = "The world needs to know.")+
        scale_color_gradient2(high = "blue",
                              mid = "grey60",
                              low = "red",
                              midpoint = 6.5,
                              limits = c(5, 8),
                              oob = scales::squish)+
        theme(legend.title = element_text())+
        guides(size = guide_legend(title = '# User Ratings',
                                   title.position = 'top'),
               color = "none")+
        xlab("Complexity")+
        ylab("Average BGG Rating")
                                    


games_model %>% 
        filter(family_animals_dinosaurs == 1) %>% 
        filter(yearpublished > 1980) %>%
        group_by(yearpublished) %>% 
        count() %>%
        ggplot(., aes(x=yearpublished,
                      y = n))+
        geom_col()+
        theme_phil()+
        geom_text(aes(label = n), 
                  vjust = 1.5, colour = "white")+
        ggtitle("How many dinosaur games were released each year?",
                subtitle = "The most important question of our time.")+
        scale_y_continuous(
                labels = scales::number_format(accuracy = 1,
                                               decimal.mark = ','))



# all families
top_families = games_model %>%
        select(starts_with("family")) %>% 
        gather() %>%
        mutate(value = replace_na(value, 0)) %>% 
        group_by(key) %>%
        summarize(n_games = sum(value)) %>%
        arrange(desc(n_games)) %>%
        head(36) %>%
        pull(key)

games_model %>% 
        select(game_id, name, yearpublished, averageweight, average, bayesaverage, usersrated, 
               starts_with("family_")) %>%
        gather("family", "value",
               -game_id,-name, -yearpublished,-averageweight,-average,-bayesaverage,-usersrated) %>%
        mutate(family = tidy_name_func(family)) %>%
        filter(value == 1) %>%
        mutate(averageweight = case_when(averageweight == 0 ~ 1, TRUE ~ averageweight)) %>%
        filter(yearpublished > 1980) %>% 
        ggplot(., aes(x=averageweight,
                    #  color = average,
                      size = usersrated,
                      label = name, y=average))+
        geom_jitter(alpha = 0.5)+
        facet_wrap(family~.,
                   ncol = 6)+
     #   geom_text_repel(size =3,
       #                 max.overlaps = 10)+
        theme_phil()+
        ggtitle("BGG Complexity vs Rating for games within Specific Families")+
        scale_color_gradient2(high = "blue",
                              mid = "grey60",
                              low = "red",
                              midpoint = 6.5,
                              limits = c(5, 8),
                              oob = scales::squish)+
        theme(legend.title = element_text())+
        guides(size = guide_legend(title = '# User Ratings',
                                   title.position = 'top'),
               color = "none")+
        xlab("Complexity")+
        ylab("Average BGG Rating")
