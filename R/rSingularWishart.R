#' Random Singular Wishart Matrix
#'
#' @inherit rWishart
#' @export
#'
#' @examples rSingularWishart(2, 5, diag(1, 20))
rSingularWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  replicate(n, rWishart::SingularWishart(df, Sigma, covariance),
            simplify = simplify)
}



#' Singular Wishart Helper Function
#'
#' @inherit rWishart
#' @export
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @examples SingularWishart(5, diag(1, 20))
SingularWishart <- function(df, Sigma, covariance = FALSE){
  singularValueDecomposition <- svd(Sigma)
  sq <- sqrt(singularValueDecomposition$d)
  sqd <- diag(sq, length(sq))
  u <- singularValueDecomposition$u
  X <- mvrnorm(n = df + 1,
               mu  = rep(0 , ncol(Sigma)),
               Sigma = Sigma)
  x <- u %*% sqd %*% t(X) %*% X %*% t(u %*% sqd)
  atr <- attributes(x)
  attributes(x) <- c(atr, df = df)
  if(covariance == TRUE){
    x <- x / df
    class(x) <- c("covariance", "matrix")
    x
  }
  x
}