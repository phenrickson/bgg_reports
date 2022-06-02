# Reviewer Collections

Users on boardgamegeek can tag games with a variety of different labels: games they own, games they've rated, games they've previously owned, games they want to play, etc. For each reviewer, we can count up the number of games for each of these different outcomes.

## Number of Games by Reviewer

```{r show table of collection}

# count games by outcome
summarized = games_and_collections_data %>%
        select(username, date, games_and_collection) %>%
        unnest(games_and_collection) %>%
        mutate_at(vars(c("prevowned","own", "owned", "rated")),
                  ~ case_when(. == 'yes' | . == 1 ~ 1,
                              TRUE ~ 0)) %>%
        mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                          TRUE ~ 0)) %>%
        mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                  TRUE ~ 0)) %>%
        select(username, date, game_id, name, own, rated, prevowned, owned_not_rated, played) %>%
        group_by(username, date) %>%
        summarize(Own = sum(own),
                  `Rated, Own, or Previously Owned` = sum(played),
                  `Previously Owned` = sum(prevowned),
                  Rated = sum(rated),
                  `Owned But Not Rated` = sum(owned_not_rated),
                  .groups = 'drop') %>%
        gather("variable", "value",
               -username, -date) %>%
        mutate(shame = case_when(variable == 'Own But Not Rated' ~ 'yes',
                                 TRUE ~ 'no')) %>%
        mutate(variable = factor(variable,
                                 levels = c("Rated",
                                            "Own",
                                            "Previously Owned",
                                            "Owned But Not Rated",
                                            "Rated, Own, or Previously Owned"))) %>%
        mutate(max = max(value)) 
```


```{r table of game outcomes by reviewer}

summarized %>%
        select(date, username, variable, value) %>%
        spread(variable, value) %>%
        arrange(desc(`Rated, Own, or Previously Owned`)) %>%
        flextable() %>%
        autofit()

```

```{r table of games outcome by reviwer plot}

library(forcats)

summarized %>%
        ggplot(., aes(y=reorder_within(username, value, variable),
                      fill = username,
                      label = value,
                      x= value)) +
        geom_col()+
        geom_text(hjust = -0.1, size = 3)+
        scale_y_reordered()+
        facet_wrap(variable ~., ncol =2, scales = "free_y")+
        theme_phil()+
        ylab("username")+
        coord_cartesian(xlim = c(0, summarized$max[1]*1.1))+
        guides(fill = "none")+
        scale_fill_username+
        #cale_fill_colorblind()+
        xlab("number of games")+
        ggtitle("Reviewer Collections")+
        #   subtitle = str_wrap("Number of games by status on BGG for each reviewer")) +
        my_caption


```

## Reviewer Collections over Time

The data doesn't indicate when a reviewer added a game to their collection, but we can plot the number of games each user has added by the year in which games were published. 

```{r look each of these over time number of owned over time, warning=F}

games_and_collections_data %>%
        select(username, date, games_and_collection) %>%
        unnest(games_and_collection) %>%
        mutate_at(vars(c("prevowned","own", "owned", "rated")),
                  ~ case_when(. == 'yes' | . == 1 ~ 1,
                              TRUE ~ 0)) %>%
        mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                          TRUE ~ 0)) %>%
        mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                  TRUE ~ 0)) %>%
        select(yearpublished, username, date, game_id, name, own, owned, rated, prevowned, owned_not_rated, played) %>%
        group_by(yearpublished, date, username) %>%
        summarize(Owned = sum(owned),
                  .groups = 'drop') %>%
        gather("variable", "value",
               -username, -date, -yearpublished) %>%
        group_by(username) %>%
        mutate(running = cumsum(value)) %>%
        filter(yearpublished > 1980 & yearpublished < year(Sys.Date())) %>%
                mutate(end = case_when(yearpublished == max(yearpublished) ~ username)) %>%
        ggplot(., aes(y=running,
                      label = end,
                      color = username,
                      x= yearpublished)) +
        geom_text_repel(
                fontface = "bold",
                size = 3,
                direction = "y",
                xlim = c(year(Sys.Date())+1, NA),
                hjust = 0,
                segment.size = .7,
                segment.alpha = .5,
                segment.linetype = "dotted",
                box.padding = .4,
                segment.curvature = -0.1,
                segment.ncp = 3,
                segment.angle = 20
                ) +
        geom_line()+
        #geom_line(stat = 'smooth',
                  # method = 'loess',
                  # formula = 'y ~ x',
                  # span = 0.15)+
        facet_wrap(variable ~.,
                   ncol =2, 
                   scales = "free_y")+
        theme_phil()+
        ylab("number of games")+
        guides(fill = "none")+
        scale_color_username+
        xlab("year published")+
        ggtitle("Games Owned by Year Published",
                subtitle = str_wrap("Filtering to games published after 1980. Displaying running total of games owned by year for each reviewer", 90))+
        guides(label = "none",
               color = "none")+
        coord_cartesian(clip = 'off',
                        xlim = c(NA, year(Sys.Date())+5))

```

A couple of things jump out from looking at their collections based on the year in which games were published. ZeeGarcia, Rahdo and Mark Bigney (Gyges) have marked the most games as owned on boardgamegeek of the listed reviewers, and Zee and Mark have probably been in the hobby the longest (or at least, the most meticulous in documenting their collections). Zee's collection really started to pick up steam in the mid 1990s, and Rahdo looks to have entered the hobby in the mid to late 2000s, which I think corresponds to when he and his wife discovered Pandemic, which sent him down the rabbit hole of board games. These three have pretty steadily marked new games as owned, while others have leveled off in the last few years (or have stopped updating their BGG collection).

We can look at the same graph to see the number of games each reviewer has rated.

```{r look each of these over time number of rated over time, warning=F}

temp_col_func<- function(x) {
        
        breaks<-seq(0, 10, 1)
        
        #  breaks = weight_deciles
        colorRamp=colorRampPalette(c("white", "deepskyblue1"))
        col_palette <- colorRamp(length(breaks))
        mycut <- cut(x, 
                     breaks = breaks,
                     include.lowest = TRUE, 
                     right=T,
                     label = FALSE)
        col_palette[mycut]
        
}

games_and_collections_data %>%
        select(username, date, games_and_collection) %>%
        unnest(games_and_collection) %>%
        mutate_at(vars(c("prevowned","own", "owned", "rated")),
                  ~ case_when(. == 'yes' | . == 1 ~ 1,
                              TRUE ~ 0)) %>%
        mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                          TRUE ~ 0)) %>%
        mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                  TRUE ~ 0)) %>%
        select(yearpublished, username, date, game_id, name, own, owned, rated, prevowned, owned_not_rated, played) %>%
        group_by(yearpublished, date, username) %>%
        summarize(Rated = sum(rated),
                  .groups = 'drop') %>%
        gather("variable", "value",
               -username, -date, -yearpublished) %>%
        group_by(username) %>%
        mutate(running = cumsum(value)) %>%
        filter(yearpublished > 1980 & yearpublished < year(Sys.Date())) %>%
        mutate(end = case_when(yearpublished == max(yearpublished) ~ username)) %>%
        ggplot(., aes(y=running,
                      label = end,
                      color = username,
                      x= yearpublished)) +
        geom_text_repel(
                fontface = "bold",
                size = 3,
                direction = "y",
                xlim = c(year(Sys.Date())+1, NA),
                hjust = 0,
                segment.size = .7,
                segment.alpha = .5,
                segment.linetype = "dotted",
                box.padding = .4,
                segment.curvature = -0.1,
                segment.ncp = 3,
                segment.angle = 20
        ) +
        geom_line()+
        #geom_line(stat = 'smooth',
        # method = 'loess',
        # formula = 'y ~ x',
        # span = 0.15)+
        facet_wrap(variable ~.,
                   ncol =2, 
                   scales = "free_y")+
        theme_phil()+
        ylab("number of games")+
        guides(fill = "none")+
        xlab("year published")+
        ggtitle("Games Rated by Year Published",
                subtitle = str_wrap("Filtering to games published after 1980. Displaying running total of games rated by year for each reviewer", 90))+
        scale_color_username+
        guides(label = "none",
               color = "none")+
        coord_cartesian(clip = 'off',
                        xlim = c(NA, year(Sys.Date())+5))

```

Zee and Mark Bigney have rated the most games by far, while Rodney Smith (WatchItPlayed) doesn't rate games that he owns.

## Overlap Between Collections

Which games have appeared the most in these reviewers' collections?
        
        ```{r games owned or previously owned the most, warning=F, message=F}

temp_col_func<- function(x) {
        
        breaks<-seq(0, 10, 1)
        
        #  breaks = weight_deciles
        colorRamp=colorRampPalette(c("white", "deepskyblue1"))
        col_palette <- colorRamp(length(breaks))
        mycut <- cut(x, 
                     breaks = breaks,
                     include.lowest = TRUE, 
                     right=T,
                     label = FALSE)
        col_palette[mycut]
        
}

games_and_collections_data %>%
        select(username, date, games_and_collection) %>%
        unnest(games_and_collection) %>%
        mutate_at(vars(c("prevowned","own", "owned", "rated")),
                  ~ case_when(. == 'yes' | . == 1 ~ 1,
                              TRUE ~ 0)) %>%
        mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                          TRUE ~ 0)) %>%
        mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                  TRUE ~ 0)) %>%
        select(yearpublished, username, date, game_id, name, own, owned, rated, prevowned, owned_not_rated, played) %>%
        filter(owned == 1) %>%
        group_by(game_id, name) %>%
        mutate(number_owned = n_distinct(username)) %>%
        select(name, username, game_id, owned, number_owned) %>%
        spread(username, owned) %>%
        rename(`In Collections` = number_owned,
               Name = name,
               ID = game_id) %>%
        mutate(ID = as.character(ID)) %>%
        arrange(desc(`In Collections`)) %>%
        filter(`In Collections`> 4) %>%
        mutate_at(.vars = reviewers_tidied,
                  ~case_when(. == 1 ~ "\U2713")) %>%
        select(ID, Name, everything()) %>%
        flextable() %>%
        autofit() %>%
        flextable::align(
                align = 'center',
                j = c('In Collections', reviewers_tidied),
                part = 'all') %>%
        color(j = reviewers_tidied,
              part = 'body',
              color = 'deepskyblue1') %>%
        bg(j = 'In Collections',
           bg = temp_col_func) %>%
        flextable::width(
                j = reviewers_tidied,
                width = 1)



```

## Highest Rated Games

What games have been the highest rated by this group of reviewers? I'll filter to games that have been rated by at least 4 of these reviewers and find the top 25 games with the highest average rating.

```{r highest rated games by users}

temp_col_func<- function(x) {
  
  breaks<-seq(1, 10, 1)
  
#  breaks = weight_deciles
  colorRamp=colorRampPalette(c("red", "white", "deepskyblue1"))
  col_palette <- colorRamp(length(breaks))
  mycut <- cut(x, 
    breaks = breaks,
    include.lowest = TRUE, 
    right=T,
    label = FALSE)
  col_palette[mycut]
  
}

games_and_collections_data %>%
        select(username, date, games_and_collection) %>%
        unnest(games_and_collection) %>%
        mutate_at(vars(c("prevowned","own", "owned", "rated")),
                  ~ case_when(. == 'yes' | . == 1 ~ 1,
                              TRUE ~ 0)) %>%
        mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                          TRUE ~ 0)) %>%
        mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                  TRUE ~ 0)) %>%
        select(yearpublished, username, date, game_id, name, own, owned, rated, rating, prevowned, owned_not_rated, played) %>%
        filter(rated == 1) %>%
        group_by(game_id, name) %>%
        mutate(number_rated = n_distinct(username)) %>%
        filter(number_rated >= 4) %>%
        mutate(mean = mean(rating, na.rm=T),
               sd = sd(rating, na.rm=T)) %>%
        select(name, username, game_id, number_rated, mean, sd, rating) %>%
        spread(username, rating) %>%
        ungroup() %>%
        arrange(desc(mean)) %>%
        head(25) %>%
        mutate_if(is.numeric, round, 1) %>%
        mutate(game_id = as.character(game_id)) %>%
        rename(ID = game_id,
               Name = name,
               Rated = number_rated,
               Mean = mean,
               SD = sd) %>%
        flextable() %>%
        bg(j =  c('Mean', reviewers_tidied[-which(reviewers_tidied == 'WatchItPlayed')]),
           bg = temp_col_func) %>%
        flextable::align(
                align = 'center',
                j = c('Rated', 'Mean', 'SD', reviewers_tidied[-which(reviewers_tidied == 'WatchItPlayed')]),
               part = 'all') %>% 
     #   autofit()
        flextable::width(
                j = c('Rated', 'Mean', 'SD', reviewers_tidied[-which(reviewers_tidied == 'WatchItPlayed')]),
                width = 1)
        
```

## Most Divisive Games

What games have been the most polarizing? This time I'll show the 25 games with the highest standard deviation in ratings.

```{r highest sd games by users}

temp_col_func<- function(x) {
        
        breaks<-seq(1, 10, 1)
        
        #  breaks = weight_deciles
        colorRamp=colorRampPalette(c("red", "white", "deepskyblue1"))
        col_palette <- colorRamp(length(breaks))
        mycut <- cut(x, 
                     breaks = breaks,
                     include.lowest = TRUE, 
                     right=T,
                     label = FALSE)
        col_palette[mycut]
        
}

games_and_collections_data %>%
        select(username, date, games_and_collection) %>%
        unnest(games_and_collection) %>%
        mutate_at(vars(c("prevowned","own", "owned", "rated")),
                  ~ case_when(. == 'yes' | . == 1 ~ 1,
                              TRUE ~ 0)) %>%
        mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                          TRUE ~ 0)) %>%
        mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                  TRUE ~ 0)) %>%
        select(yearpublished, username, date, game_id, name, own, owned, rated, rating, prevowned, owned_not_rated, played) %>%
        filter(rated == 1) %>%
        group_by(game_id, name) %>%
        mutate(number_rated = n_distinct(username)) %>%
        filter(number_rated >= 4) %>%
        mutate(mean = mean(rating, na.rm=T),
               sd = sd(rating, na.rm=T)) %>%
        select(name, username, game_id, number_rated, mean, sd, rating) %>%
        spread(username, rating) %>%
        ungroup() %>%
        arrange(desc(sd)) %>%
        head(25) %>%
        mutate_if(is.numeric, round, 1) %>%
        mutate(game_id = as.character(game_id)) %>%
        rename(ID = game_id,
               Name = name,
               Rated = number_rated,
               Mean = mean,
               SD = sd) %>%
        flextable() %>%
        bg(j =  c('Mean', reviewers_tidied[-which(reviewers_tidied == 'WatchItPlayed')]),
           bg = temp_col_func) %>%
        flextable::align(
                align = 'center',
                j = c('Rated', 'Mean', 'SD', reviewers_tidied[-which(reviewers_tidied == 'WatchItPlayed')]),
                part = 'all') %>% 
        #   autofit()
        flextable::width(
                j = c('Rated', 'Mean', 'SD', reviewers_tidied[-which(reviewers_tidied == 'WatchItPlayed')]),
                width = 1)

```

## Reviewer Ratings and BGG

How does each reviewer's rating compare to the BGG community rating?

```{r user collections, warning=F, message=F}

set.seed(1999)
pos1 = position_jitternormal()

# plot rating vs bgg rating
games_and_collections_data %>%
        select(username, date, games_and_collection) %>%
        unnest(games_and_collection) %>%
        mutate_at(vars(c("prevowned","own", "owned", "rated")),
                  ~ case_when(. == 'yes' | . == 1 ~ 1,
                              TRUE ~ 0)) %>%
        mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                          TRUE ~ 0)) %>%
        mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                  TRUE ~ 0)) %>%
        filter(rated == 1) %>%
        select(username, date, yearpublished, game_id, name, rating,  average, averageweight) %>%
        ggplot(., aes(average,
                      y = rating,
                      label = name))+
        geom_point(pos = pos1,
                   alpha = 0.25)+
        geom_text(size =2.5, vjust=-1,
                  check_overlap=T)+
        facet_wrap(username ~.,
                   ncol =3)+
        theme_phil()+
        geom_smooth(formula = 'y ~ x',
                    method = 'loess',
                    se = F)+
        coord_cartesian(
                #ylim = c(-0.5, 11),
                        xlim = c(-0.5, 11))+
                stat_cor(p.accuracy = 0.01,
                 col = 'blue',
                 label.y = 1,
                 label.x = 1)+
        ylab("reviewer rating")+
        xlab("bgg rating")

```

Similarly, what is the relationship between each reviewer's rating and the complexity of the game (as voted by bgg)?
        
        ```{r reviewer ratings vs average ewight, warning=F, message=F}

set.seed(1999)
pos1 = position_jitternormal()

# plot rating vs bgg rating
games_and_collections_data %>%
        select(username, date, games_and_collection) %>%
        unnest(games_and_collection) %>%
        mutate_at(vars(c("prevowned","own", "owned", "rated")),
                  ~ case_when(. == 'yes' | . == 1 ~ 1,
                              TRUE ~ 0)) %>%
        mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                          TRUE ~ 0)) %>%
        mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                  TRUE ~ 0)) %>%
        filter(rated == 1) %>%
        select(username, date, yearpublished, game_id, name, rating,  average, averageweight) %>%
        ggplot(., aes(averageweight,
                      y = rating,
                      label = name))+
        geom_point(pos = pos1,
                   alpha = 0.25)+
        geom_text(size =2.5, vjust=-1,
                  check_overlap=T)+
        facet_wrap(username ~.,
                   ncol =3)+
        theme_phil()+
        geom_smooth(formula = 'y ~ x',
                    method = 'loess',
                    se = F)+
        coord_cartesian(ylim = c(-0.5, 11),
                        xlim = c(-0.5, 5.5))+
        stat_cor(p.accuracy = 0.01,
                 col = 'blue',
                 label.y = 1,
                 label.x = 1.5)+
        ylab("reviewer rating")+
        xlab("bgg complexity")

```

We can also look at the distribution of games owned by each reviewer by bgg complexity. Whose collection tends to have the most complex games?
        
        ```{r show number of games owned by bgg weight, warning=F, message=F, results = 'hide'}

suppressMessages ({
        print(games_and_collections_data %>%
                      select(username, date, games_and_collection) %>%
                      unnest(games_and_collection) %>%
                      mutate_at(vars(c("prevowned","own", "owned", "rated")),
                                ~ case_when(. == 'yes' | . == 1 ~ 1,
                                            TRUE ~ 0)) %>%
                      mutate(owned_not_rated= case_when(rated == 0 & owned == 1 ~ 1,
                                                        TRUE ~ 0)) %>%
                      mutate(played = case_when(own == 1 | rated == 1 | prevowned == 1 ~ 1,
                                                TRUE ~ 0)) %>%
                      select(username, date, yearpublished, owned, game_id, name, rating,  average, averageweight) %>%
                      filter(owned == 1) %>%
                      group_by(username) %>%
                      mutate(user_median = median(averageweight, na.rm=T)) %>%
                      ungroup() %>%
                      ggplot(., aes(x=averageweight,
                                    fill = username,
                                    y = reorder(username, user_median)))+
                      stat_density_ridges(color = 'white',
                                          alpha = 0.8,
                                          quantile_lines = T,
                                          quantile_fun = median)+
                      scale_fill_username+
                      theme_phil()+
                      guides(color = "none",
                             fill = "none")+
                      ylab("username")+
                      #      ggtitle("Reviewer Collections by Complexity")+
                      xlab("complexity")+
                      ylab("user collection")+
                      ggtitle("Reviewer Collections by BGG Complexity",
                              subtitle = str_wrap("Displaying all games owned by each reviewer. White line indicates median complexity."))
        )
})

```

How do reviewer predictions compare? I'll look at the correlation (Spearman) between the probabilities for each reviewer.

```{r compare correlation}

games_upcoming_probs %>%
        select(yearpublished, game_id, name, .pred_yes, username) %>%
        filter(!(game_id %in% upcoming_flags$game_id)) %>%
        spread(username, .pred_yes) %>%
        select(all_of(reviewers_tidied)) %>%
        cor(method = 'spearman') %>%
        ggcorrplot(hc.order = TRUE, 
                   lab = T,
                 #  type = 'lower',
                   outline.color = "white")+
        theme(axis.text.x= element_text(size = 8,
                                       angle = 45))+
        theme(axis.text.y = element_text(size = 8))

```

This would indicate that Ava's preferences align most closely with Quinns and aboardgamebarrage, that Mark Bigney's tastes are most similar to Walker and J_3MBG, etc.


```{r plot predicted probabilities, warning=F, message=F, eval=F, fig.height=8, fig.width=8}

library(GGally)

assignInNamespace("ggally_cor", ggally_cor, "GGally")

suppressMessages({
        
        print(
                games_upcoming_probs %>%
                select(yearpublished, game_id, name, .pred_yes, username) %>%
                filter(!(game_id %in% upcoming_flags$game_id)) %>%
                spread(username, .pred_yes) %>%
                select(all_of(reviewers_tidied)) %>%
                ggpairs(diag=list(continuous="bar"),
                        upper = list(continuous = wrap("cor", method = "spearman")),
                        aes(alpha = 0.25))+
                theme_phil()+
                        theme(legend.position = "none", 
                              panel.grid.major = element_blank(), 
                              axis.ticks = element_blank(), 
                              panel.border = element_rect(linetype = "dashed", colour = "black", fill = NA))
                )
        
})

```

