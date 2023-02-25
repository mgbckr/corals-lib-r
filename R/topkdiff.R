# TODO: check indexing, write test
#' @export
topkdiff_matrix <- function(X1, X2, Y1=NULL, Y2=NULL, k=NULL) {
    
    diff <- as.vector(cor_matrix_symmetrical(X1, Y1)) - as.vector(cor_matrix_symmetrical(X2, Y2))

    if (is.null(k)) {
        k = length(cor1) * 0.01  # one percent of all correlations
    }
    
    ord <- order(-abs(diff))[1:k]

    m = dim(X1)[[1]]
    r = ((ord-1) %% m) + 1
    c = floor((ord-1) / m) + 1

    return(list(diff=diff[ord], idx.r=r, idx.c=c))
}