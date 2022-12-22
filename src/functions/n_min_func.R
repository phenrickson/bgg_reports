# custom n.min function
n_min_func <- function(row, n) {
        neighbors = order(row)[1:n]
        data.frame(neighbor = neighbors,
                   dist = row[neighbors])
}
