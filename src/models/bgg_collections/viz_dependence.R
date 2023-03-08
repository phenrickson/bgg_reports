# function to visualize partial dependence of a variable for any wflow
viz_dependence = function(wflow,
                          data,
                          variable,
                          center = T,
                          length.out = 10,
                          n = 1000,
                          plot = T) {
        
        # get range of variable in training set
        range = range(data[,variable], na.rm = T)
        
        # numeric range from min to max
        num = seq(min(range), max(range), length.out = 10)
        
        # sample data to specific size
        if (nrow(data) >= n) {
                sample = sample_n(data, n) %>%
                        mutate(.row = row_number())
        } else {
                sample = data %>%
                        mutate(.row = row_number())
        }
        
        
        # pred at center of variable
        means = wflow %>%
                augment(data %>%
                                mutate(!!variable:= variable_val,
                                       type = 'prob')) %>%
                summarize(mean = mean(.pred_yes)) %>%
                pull(mean)
        
        # predict at each value
        preds = map(num,
                    ~  wflow %>%
                            augment(sample %>%
                                            mutate(!!variable:= .x,
                                                   type = 'prob'))) %>%
                # bind together
                bind_rows() 
        
        # create plot if plot =T
        if (plot == T) {
                
                # for ggplot
                column <- ensym(variable)
                
                # plot
                preds %>%
                        ggplot(aes(x = !!column,
                                   group = .row,
                                   y = .pred_yes))+
                        geom_line(alpha = 0.25)+
                        # add rug
                        geom_rug(data = data,
                                 alpha = 1/2, 
                                 side = 'b',
                                 position = "jitter")
                
                
        } else if (plot != T) {
                
                # return preds
                preds
        }
        
}

viz_dependence(wflow,
               train_games,
               'number_mechanics',
               variable_val = 10,
               length.out = 10,
               n = 1000,
               plot = T)


        preds %>%
                ggplot(aes(x=number_mechanics,
                   group = .row,
                   y=.pred_yes))+
        geom_line()
               


wflow = last_fits %>%
        filter(wflow_id == 'trees_lightgbm') %>%
        pluck(".workflow", 1)

# viz redictors
wflow = glmnet_fit
var = 'word_count'
range = range(train_games[,var], na.rm = T)
num = seq(min(range), max(range), length.out = 10)

preds = map(num,
            ~  wflow %>%
                    augment(train_games %>%
                                    mutate(!!var:= as.integer(.x),
                                           type = 'prob')) %>%
                    select(.pred_yes, game_id, name, !!var)) %>%
        bind_rows() 

column <- ensym(var)

preds %>%
        ggplot(aes(x = !!column,
                   group = game_id,
                   y = .pred_yes))+
        geom_line(alpha = 0.25)
