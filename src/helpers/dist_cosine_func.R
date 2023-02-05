# cosine similarity
dist_cosine_func = function(input_mat) {
        
        Matrix <- as.matrix(input_mat)
        sim <- Matrix / sqrt(rowSums(Matrix * Matrix))
        out <- sim %*% t(sim)
        
}