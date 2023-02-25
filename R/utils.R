#' @export
preprocess_XY <- function(X, Y=NULL, avoid_copy=FALSE) {

    # TODO: make faster?
    # X <- sweep(X, 2, colMeans(X))
    # X <- X %*% diag(1/sqrt(colSums(X^2)))

    X <- scale(X) / sqrt(dim(X)[1] - 1)
    if (is.null(Y)) {
        if (avoid_copy) {
            Y <- X
        } else {
            Y <- X / 1
        }
    }
    return(list(X=X, Y=Y))
}
