# get distances
get_dist_func = function(input_data, select_method) {
        
        dist(input_data %>%
                     select_if(is.numeric) %>%
                     as.matrix(),
             method=paste0(select_method)) %>%
                as.matrix()
        
}