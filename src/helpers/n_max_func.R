# custom n.min function
n_max_func <- function(row, n) {
        neighbors = rev(order(row))[1:n]
        data.frame(neighbor = neighbors,
                   dist = row[neighbors])
}
