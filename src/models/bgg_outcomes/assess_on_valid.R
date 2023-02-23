# collect results and finalize

# read hurdle model
model_board = board_folder(here::here("models", "board"))

# get 
load(here::here("data", "local", "analysis_games_tables.Rdata"))

# functions
source(here::here("src", "features", "make_features_functions.R"))
source(here::here("src", "helpers", "theme_phil.R"))

# # load hurdle model
hurdle_model = vetiver::vetiver_pin_read(board = model_board,
                                    name = "hurdle_model")

# predict games in validation set with hurdle model
estimate_bayesaverage = function(data,
                                votes = 2000) {
        data %>%
                mutate(.pred_bayesaverage = ((2000*5.5)+exp(usersrated)*average) /
                               (2000 + exp(usersrated)))

}

# assign every game a probability of getting a geek rating
valid_hurdle = hurdle_model %>%
        # replace averageweight with predicted
        augment(
                # prep for hurdle model
                hurdle_prep(
                        # predict averageweight with model
                        impute_averageweight(valid)
                )
        ) %>%
        # then, assign them a prediction 
        mutate(.pred_class = factor(case_when(.pred_yes > .15 ~ 'yes',
                                              TRUE ~ 'no'),
                                    levels = c("no", "yes")),
               threshold = factor(case_when(!is.na(bayesaverage) ~ 'yes',
                                            TRUE ~ 'no'),
                                  levels = c("no", "yes")))

# view predictions vs actual for averageweight
averageweight_model %>%
        augment(valid) %>%
        transmute(game_id, 
                  yearpublished, 
                  name,
                  usersrated,
                  .pred_averageweight = .pred,
                  averageweight) %>%
        filter(!is.na(averageweight)) %>%
        ggplot(aes(x=.pred_averageweight,
                   size = usersrated,
                   y= averageweight))+
        geom_point()+
        coord_obs_pred()+
        theme_phil()+
        theme(legend.title = element_text())+
        guides(size = guide_legend(title.position = 'top'))


# now predict games in validation set
valid_preds =
        train_models %>%
        mutate(preds = map(workflow,
                           ~ .x %>%
                                   augment(valid_hurdle) %>%
                                   select(game_id,
                                          yearpublished,
                                          name, 
                                          .pred)))

# pivot wider
valid_preds_and_actual =
        valid_preds %>%
        select(outcome, preds) %>%
        unnest(preds) %>%
        pivot_wider(id_cols = c("game_id", "yearpublished", "name"),
                    names_from = c("outcome"),
                    values_from = c(".pred")) %>%
        # compute geek rating indirectly
        mutate(bayesaverage_indirect = 
                       ((2000*5.5)+exp(usersrated)*average) /
                       (2000 + exp(usersrated))) %>%
        pivot_longer(cols = -c(game_id, yearpublished, name),
                     names_to = c("outcome"),
                     values_to = c(".pred")) %>%
        separate(outcome, into = c("outcome", "type"), 
                 sep = "_",
                 fill = "right",
                 extra = "drop") %>%
        mutate(type = replace_na(type, 'direct')) %>%
        # now join with actual
        left_join(.,
                  valid_hurdle %>%
                          transmute(game_id,
                                    .pred_yes,
                                    usersrated = log_usersrated,
                                    average = average,
                                    bayesaverage = bayesaverage) %>%
                          pivot_longer(cols = -c(game_id, .pred_yes),
                                       names_to = c("outcome"),
                                       values_to = c("actual")),
                  by = c("game_id", "outcome"))

# plot for games that have a geek rating
valid_preds_and_actual %>%
        arrange(desc(actual)) %>% 
        # filter(game_id %in% (valid %>%
        #                              filter(!is.na(bayesaverage)) %>% 
        #                              pull(game_id))) %>% 
        ggplot(aes(x=.pred, 
                   color = .pred_yes,
                   y= actual))+
        facet_wrap(~type + outcome,
                   scales = "free") +
        geom_point(size = 0.75,
                   alpha = 0.8)+
        geom_abline()+
        theme_phil()+
        scale_color_gradient(low = 'white', 
                                       high = 'navy')+
        guides(color = guide_colorbar(barwidth = 10,
                                      barheight = 0.5,
                                      title.position = 'top'))+
        theme(legend.title = element_text())
        
# compute on bayesaverage
valid_preds_and_actual %>%
        mutate(actual = case_when(outcome == 'bayesaverage' & is.na(actual) ~ 5.5,
                                 TRUE ~ actual)) %>%
        filter(outcome == 'bayesaverage') %>%
        group_by(outcome, type) %>%
        reg_metrics(truth = actual,
                    estimate = .pred) %>%
        arrange(.metric) %>%
        spread(.metric, .estimate)

# top games according to model
valid_preds_and_actual %>% 
        filter(outcome == 'bayesaverage') %>%
        select(game_id, yearpublished, name, outcome, type, .pred_yes, .pred, actual) %>%
        spread(type, .pred) %>%
        arrange(desc(indirect)) 

# 
results_board = pins::board_folder(here::here("models", "results"), versioned = T)
pin_write(results_board,
          valid_preds_and_actual %>%
                  nest(data = c(-outcome, -type)),
          name = "outcomes_valid_results",
          description = paste("results on validation set from models trained through", end_train_year))


