library(ggforce)
library(splines)

source(here::here("src", "helpers", "theme_phil.R"))

# number of mechanics
plot_smooth = function(var,
                       df) {
        
        train %>%
                mutate(yearpublished = case_when(yearpublished < 1900 ~ 1900,
                                                 TRUE ~ yearpublished)) %>%
                mutate(playingtime = log1p(playingtime)) %>%
                mutate(time_per_player = playingtime / maxplayers) %>%
                mutate(time_per_player = log1p(time_per_player)) %>%
                mutate(number_mechanics = rowSums(across(starts_with("mec_")))) %>%
                mutate(number_categories = rowSums(across(starts_with("cat_")))) %>%
                mutate(usersrated = log(usersrated)) %>%
                select(game_id,
                          name,
                          all_of(var),
                          average,
                          averageweight,
                          usersrated)  %>%
                ggplot(aes(x=.panel_x,
                           y=.panel_y))+
                geom_autopoint(size = 0.5)+
                geom_smooth(
                        method = lm,
                        formula = y ~ ns(x, df = df),
                        color = "blue",
                        se = T,
                ) +
                facet_matrix(vars(averageweight, average, usersrated),
                             vars(all_of(var)))+
                theme_bw()+
                ggtitle(paste(df, "spline terms"))
}
              

plot_smooth("number_mechanics", 5)
plot_smooth("number_categories", 5)
plot_smooth("yearpublished", 5)
plot_smooth("playingtime", 3)
plot_smooth("time_per_player", 3)

# principal components
pca %>%
        set_names(., gsub("PC0", "PC", gsub("PC0", "PC", names(.)))) %>%
        select(game_id, name, yearpublished,PC2, PC3, PC4) %>%
        ggplot(aes(x=.panel_x,
                   y=.panel_y))+
        geom_autopoint(size = 0.5,
                       alpha = 0.5)+
        geom_autodensity()+
        facet_matrix(vars(-game_id, -name, -yearpublished),
                     vars(-game_id, -name, -yearpublished),
                     layer.diag = 2)+
        theme_bw()

        
        
