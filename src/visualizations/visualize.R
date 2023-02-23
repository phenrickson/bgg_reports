library(ggforce)
library(splines)

source(here::here("src", "helpers", "theme_phil.R"))

# plot feature with splines
plot_smooth = function(var,
                       degrees) {
        
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
                        formula = y ~ ns(x, df = degrees),
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
# # not run
# pca = base_recipe_func("average") %>%
#         preproc_recipe_func() %>%
#         splines_recipe_func() %>%
#         # add specific update for averageweight
#         update_role("averageweight",
#                     new_role = "predictor") %>%
#         step_impute_median(averageweight) %>%
#         # normalize
#         norm_recipe_func() %>%
#         # pca
#         pca_recipe_func() %>%
#         # prep on train
#         prep(train)

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

        
## shapley

# get a wflow
wflow = impute_wflows %>%
        filter(wflow_id == 'minimal_xgb') %>%
        pluck(".workflow", 1)

# extract mold
mold = wflow %>%
        extract_mold()

# extract model
mod = wflow %>%
        extract_fit_engine()

# extract recipe template
template = wflow %>%
        extract_preprocessor() %$%
        template

# extract training set
orig = 
        bind_cols(
                # outcome
                wflow %>%
                        extract_mold() %$%
                        outcomes,
                # predictors
                wflow %>%
                        extract_mold() %$%
                        predictors,
                # ids
                wflow %>%
                        extract_mold() %$%
                        extras %$%
                        roles %$%
                        id) %>%
        select(names(template))

# get outcome var
outcome =   wflow %>%
        extract_mold() %$%
        outcome
        
# get list of features used in model
features =
        mod %$%
        feature_names

# matrix of features used in model
mat = wflow %>% 
        extract_mold() %$% 
        predictors %>%
        as.matrix()

# run shap
shap = fastshap::explain(mod,
                         exact = T,
                         newdata = mat)

# find 40 vars with largest effect
top_vars = shap %>% 
        colSums() %>% 
        as.data.frame() %>% rownames_to_column() %>% slice_max(abs(.), n =40)
        
# get training set predictions
dat = impute

# viw all
preds = wflow %>%
        augment(dat) %>%
        mutate(.row = row_number()) %>%
        select(game_id, name, .pred, all_of(outcome)) %>%
        bind_cols(., shap %>%
                          as_tibble)

# display all shap
preds %>%
        sample_n(1000) %>%
        pivot_longer(cols = -c(game_id, name, .pred, all_of(outcome)),
                     names_to = "feature",
                     values_to = "value") %>%
        # mutate 
        mutate(feature = case_when(feature %in% top_vars$rowname ~ feature,
                                   TRUE ~ 'other')) %>%
        # reorder
        mutate(feature = factor(feature,
                                levels = rev(c(top_vars$rowname, 'other')))) %>%
        # median
        ggplot(aes(x=value,
                   color = value,
                   y=feature))+
        geom_point(position = ggforce::position_jitternormal(sd_x = 0))+
        theme_minimal()
        
preds %>%
        filter(game_id == sample(dat$game_id,1)) %>%
        pivot_longer(cols = -c(game_id, name, .pred, averageweight),
                     names_to = "feature",
                     values_to = "value") %>%
        slice_max(abs(value), n=25, with_ties = F) %>%
      #  mutate(feature = abbreviate(str_to_title(gsub("_", " ", feature)), minlength = 25)) %>%
        mutate(subtitle =
                       paste(
                               paste("Average Weight"),
                               paste("Estimated:", round(.pred, 2)),
                               paste("Actual:", round(averageweight, 2)),
                               sep = "\n")) %>%
        {
                ggplot(., aes(x=value,
                              fill = value,
                              y = reorder(feature, value)))+
                        geom_col()+
                        ylab("Feature")+
                        scale_fill_gradient2(low = "red",
                                             mid = "grey60",
                                             high = "blue",
                                             midpoint = 0,
                                             limits = c(-0.1, 0.1),
                                             oob = scales::squish)+
                        facet_wrap(~ subtitle)+
                        labs(title = paste(paste("Game:", .$name),
                                              paste("ID:",
                                                    .$game_id),
                                              sep="\n"),
                             subtitle = str_wrap("Displaying Shapley Values to identify which features were the most influential for each model's prediction. Features that increased a game's prediction are positive (in blue), while features that decreased a prediction are negative (in red).", 90))
        }+
        theme_minimal()+
        theme(plot.title = element_text(size=12),
              plot.subtitle = element_text(size = 8),
              strip.text.x = element_text(size = 10))



# view penalty
impute_race_res %>%
        unnest(result) %>% 
        filter(wflow_id == 'norm_glmnet') %>%
        select(id, wflow_id, .metrics) %>% 
        unnest(.metrics) %>%
        ggplot(aes(x=log(penalty),
                   color = wflow_id,
                   by = id,
                   y = .estimate))+
        geom_line()+
        facet_wrap(.metric ~.,
                   scales = "free_y")+
        scale_color_brewer(palette = 'Set1')

# simulateX <- function(object, nsim = 1, seed = NULL, X, ...) {
#         object$fitted.values <- predict(object, X)
#         simulate(object = object, nsim = nsim, seed = seed, ...)
# }
# 
# sims = foo %>% 
#         extract_fit_engine() %>%
#         simulate(nsim = 1000) %>%
#         tibble() 
# 
# sims %>%
#         bind_cols(., dat) %>% 
#         filter(game_id == sample(dat$game_id, 1)) %>% 
#         select(starts_with("sim_"),
#                game_id, averageweight) %>%
#         pivot_longer(cols = -c(game_id, averageweight)) %>% { ggplot(., aes(x=value))+geom_histogram(bins = 80)+geom_vline(xintercept = .$averageweight, color = 'red')}+coord_cartesian(xlim = c(1, 5))
# 
        
# missingness -------------------------------------------------------------

# missingness in training set
# train %>%
#         summarise(across(everything(), ~ sum(is.na(.)))) %>%
#         pivot_longer(cols = everything()) %>%
#         arrange(desc(value))
# 
# # viz missingness
# naniar::vis_miss(train %>%
#                          arrange(game_id) %>%
#                          select(-load_ts, -description) %>%
#                          select(-starts_with("pub_"),
#                                 -starts_with("cat_"),
#                                 -starts_with("fam_"),
#                                 -starts_with("des_"),
#                                 -starts_with("mec_"),
#                                 -starts_with("art_")),
#                  warn_large_data = F)

