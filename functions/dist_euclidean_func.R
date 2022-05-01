# euclidean distance
dist_euclidean_func = function(input_mat) {
        
        dist =  dist(input_mat,
                     method = "euclidean")
        out = dist %>%
                as.matrix()
        
}
