#' @export
cor_matrix_symmetrical <- function(X, Y=NULL, avoid_copy=FALSE) {
    m <- preprocess_XY(X, Y, avoid_copy=avoid_copy)
    return(t(m$X)%*%m$Y)
}
