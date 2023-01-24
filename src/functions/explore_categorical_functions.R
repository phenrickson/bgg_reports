# function for names

# function for making categorical dummies from type and value
tidy_categorical_variables = function(x) {

        x %>%
                mutate(abbrev= paste(substr(type, 1, 3)),
                       level = tolower(gsub("\\)", "", gsub("\\(", "", gsub("[[:space:]]", "_", gsub("[[:punct:]]\\s+", "", value)))))) %>%
                mutate(value = paste(abbrev, level, sep="_"),
                       has_value = 1)
}

# standardize names for bgg outcomes
tidy_names_func = function(x, log=T) {

        x = gsub("\\baverage\\b", "average rating", x)
        x = gsub("\\baverageweight\\b", "average weight", x)
        x = gsub("\\bbayesaverage\\b", "geek rating", x)

        if (log == T) {
                x = gsub("\\busersrated\\b", "user ratings (logged)", x)
        } else {
                x = gsub("\\busersrated\\b", "user ratings", x)
        }

        return(x)
}

# use ggforce to create categorical plot
plot_games_by_categorical = function(games_categorical,
                                     alpha = .05,
                         n_categories) {

        # join categories up with bgg outcomes
        join = games_categorical %>%
                right_join(., tidy_games %>%
                                   select(game_id,
                                          name,
                                          averageweight,
                                          bayesaverage,
                                          average,
                                          usersrated),
                           by = c("game_id"))  %>%
                filter(!is.na(value))

        # get top n categories
        top_n = join %>%
                group_by(value, id) %>%
                summarize(num_games = n_distinct(game_id),
                          .groups = 'drop') %>%
                slice_max(num_games, n=n_categories, with_ties = F) %>%
                pull(id)

        # plot
        join %>%
                # filter to categories in top n
                filter(id %in% top_n) %>%
                transmute(value,
                          id,
                          game_id,
                          name,
                          averageweight,
                          average,
                          usersrated = log(usersrated)) %>%
                ungroup() %>%
                set_names(., tidy_names_func(names(.))) %>%
                ggplot(aes(x=.panel_x,
                           y=.panel_y))+
                geom_autopoint(alpha = alpha,
                               size = 0.25)+
                facet_matrix(vars(value),
                             vars(-game_id, -name, -id, -value),
                             grid.y.diag = F)+
                theme_bw()+
                theme(panel.grid.major = element_blank())+
                my_caption+
                scale_color_brewer(palette = 'Set2')+
                scale_fill_brewer(palette = 'Set2')+
                guides(color = 'none',
                       fill = 'none')


}

summarize_games_by_categorical = function(games_categorical) {

        # join categories up with bgg outcomes
        join = games_categorical %>%
                right_join(., tidy_games %>%
                                   select(game_id,
                                          name,
                                          averageweight,
                                          bayesaverage,
                                          average,
                                          usersrated),
                           by = c("game_id"))  %>%
                filter(!is.na(value))

        # get summaries by category
        summarized = join %>%
                group_by(type, value, id) %>%
                mutate(num_games = n_distinct(game_id),
                       .groups = 'drop') %>%
                group_by(type, value, id, num_games) %>%
                summarize_at(c("averageweight",
                               "bayesaverage",
                               "average",
                               "usersrated"),
                             list(mean = ~ mean(., na.rm=T),
                                  median = ~ median(., na.rm=T),
                                  sd = ~ sd(., na.rm=T))) %>%
                ungroup() %>%
                select(type, value, id, num_games, everything())

        # return
        return(list("data" = join,
                    "summary" = summarized))

}


# function to take categorical summary and make a table
make_categorical_summary_table = function(summary_data) {

        # take input and get mean
        summary =
                summary_data %$%
                summary %>%
                mutate_if(is.numeric, round, 2) %>%
                select(type, value, id, num_games, contains("_mean")) %>%
                set_names(., gsub("_mean", "", names(.))) %>%
                select(type, value, num_games, averageweight, average, bayesaverage, usersrated) %>%
                mutate(usersrated = round(usersrated, 0),
                       num_games = as.integer(num_games))

        # get table variable
        table_variable = stringr::str_to_title(summary$type[1])

        # create quantile gradients by outcome
        seq_averageweight = c(0,
                              quantile(summary$averageweight, seq(0, 1, .05)),
                              max(summary$averageweight, na.rm=T))

        seq_average = c(min(summary$average,na.rm=T)-.1,
                        quantile(summary$average, seq(0, 1, .1)),
                        max(summary$average, na.rm=T)+.5)

        seq_bayesaverage = c(min(summary$bayesaverage,na.rm=T)-.1,
                             quantile(summary$bayesaverage, seq(0, 1, .05)),
                             max(summary$bayesaverage, na.rm=T)+.5)

        seq_usersrated = c(0,
                           quantile(summary$usersrated, seq(0, 1, .05)),
                           max(summary$usersrated, na.rm=T)+10000)



        # custom table container to add headers
        sketch = htmltools::withTags(table(
                # class = 'display',
                thead(
                        tr(
                                th(rowspan = 2, table_variable),
                                th(rowspan = 2, 'Games'),
                                th(colspan = 4, paste('Mean of Games with', table_variable))
                        ),
                        tr(
                                lapply(c("Weight", "Average", "Geek", "Ratings"), th)
                        )
                )
        )
        )

        # create table
        summary %>%
                transmute(Mechanic = value,
                          Games = num_games,
                          Weight = averageweight,
                          Average = average,
                          Geek = bayesaverage,
                          Ratings = usersrated
                ) %>%
                arrange(desc(Games)) %>%
                datatable(escape=F,
                          rownames = F,
                          extensions = c('Responsive'),
                          #    caption = 'ZoomInfo Companies - Client Probabilities by COE',
                          container = sketch,
                          filter = list(position = 'top'),
                          options = list(pageLength = 15,
                                         initComplete = htmlwidgets::JS(
                                                 "function(settings, json) {",
                                                 paste0("$(this.api().table().container()).css({'font-size': '", '8pt', "'});"),
                                                 "}"),
                                         scrollX=F,
                                         columnDefs = list(
                                                 list(className = 'dt-center',
                                                      visible=T,
                                                      targets=c("Mechanic",
                                                                "Games",
                                                                "Weight",
                                                                "Average",
                                                                "Geek",
                                                                "Ratings"))
                                         )
                          )
                ) %>%
                # average
                formatStyle(c(
                        "Average"),
                        #     color = 'white',
                        backgroundColor = styleInterval(seq_average,
                                                        blueRampSeq(length(seq_average)+1))) %>%
                # bayesaverage
                formatStyle(c(
                        "Geek"),
                        #     blue = 'white',
                        backgroundColor = styleInterval(seq_bayesaverage,
                                                        blueRampSeq(length(seq_bayesaverage)+1))) %>%
                # usersrated
                formatStyle(c(
                        "Ratings"),
                        #     blue = 'white',
                        backgroundColor = styleInterval(seq_usersrated,
                                                        blueRampSeq(length(seq_usersrated)+1))) %>%
                # averageweight
                formatStyle(c(
                        "Weight"),
                        #     blue = 'white',
                        backgroundColor = styleInterval(seq_averageweight,
                                                        redRampSeq(length(seq_averageweight)+1)))



}

