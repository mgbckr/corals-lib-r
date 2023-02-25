# TODO: check indexing, write test
#' @export
topk_matrix <- function(X, Y=NULL, k=NULL) {
    
    cor <- as.vector(cor_matrix_symmetrical(X, Y))
    
    if (is.null(k)) {
        k = length(cor) * 0.01  # one percent of all correlations
    }
    
    print(length(cor))
    ord <- order(-abs(cor))[1:k]

    m = dim(X)[[1]]
    r = ((ord-1) %% m) + 1
    c = floor((ord-1) / m) + 1

    return(list(cor=cor[ord], idx.r=r, idx.c=c))
}