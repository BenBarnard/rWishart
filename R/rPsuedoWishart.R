#' Title
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
SingularWishFunc <- function(df, Sigma, cholesky, covariance){
  X <- mvrnorm(n = df + 1,
               mu  = rep(0 , ncol(Sigma)),
               Sigma = Sigma)
  x <- cholesky %*% t(X) %*% X %*% t(cholesky)
  atr <- attributes(x)
  attributes(x) <- c(atr, n = df + 1)
  if(covariance == TRUE){
    x <- x / df
    class(x) <- c("covariance", "matrix")
    x
  }
  x
}