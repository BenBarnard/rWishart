#' Random Psuedo Wishart Matrix
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
#' @examples PsuedoWishart(5, diag(1, 20))
PsuedoWishart <- function(df, Sigma, covariance = FALSE){
  cholesky <- chol(Sigma)
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