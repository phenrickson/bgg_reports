# get neighbors from a distance matrix
find_neighbors_min_func = function(input_dist, num_neighbors) {
        apply(input_dist, 1, n_min_func, n = num_neighbors) %>%
                rbindlist(idcol = T) %>%
                set_names(., c(".row", ".row_neighbor", "dist")) %>%
                mutate(.row = as.integer(.row),
                       .row_neighbor = as.integer(.row_neighbor))
        #   filter(dist > 0)
}
