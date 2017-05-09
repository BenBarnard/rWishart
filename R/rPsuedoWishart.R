#' Random Psuedo Wishart Matrix
#'
#' Generate \code{n} random matrices, distributed according to the Wishart distribution with parameters \code{Sigma} and \code{df}, W_p(Sigma, df).
#'
#' @inherit rWishart
#' @export
#'
#' @examples rPsuedoWishart(2, 5, diag(1, 20))
rPsuedoWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  replicate(n, rWishart::PsuedoWishart(df, Sigma, covariance),
            simplify = simplify)
}



#' Psuedo Wishart Helper Function
#'
#' @inherit rWishart
#' @export
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @importFrom lazyeval f_unwrap
#' @examples PsuedoWishart(5, diag(1, 20))
PsuedoWishart <- function(df, Sigma, covariance = FALSE){
  df <- df
  cholesky <- chol(Sigma)
  X <- mvrnorm(n = df,
               mu  = rep(0 , ncol(Sigma)),
               Sigma = Sigma)
  x <- cholesky %*% t(X) %*% X %*% t(cholesky)
  atr <- attributes(x)
  attributes(x) <- c(atr, df = f_unwrap(~ df))
  if(covariance == TRUE){
    x <- x / df
    class(x) <- c("covariance", "matrix")
    x
  }
  x
}