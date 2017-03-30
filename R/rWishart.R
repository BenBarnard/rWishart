#' Random Wishart Distributed Matrices
#'
#' @param n
#' @param df
#' @param Sigma
#'
#' @return
#' @export
#' @importFrom stats rWishart
#' @importFrom MASS mvrnorm
#'
#' @examples
rWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  if(df >= ncol(Sigma)){
    ls <- replicate(n,
                    wishart::WishFunc(df, Sigma, covariance),
                    simplify = simplify)
  }else{
    cholesky <- chol(Sigma)
    ls <- replicate(n,
                    wishart::SingularWishFunc(df, Sigma, cholesky, covariance),
                    simplify = simplify)
  }
  ls
}

#' Title
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
WishFunc <- function(df, Sigma, covariance){
  x <- stats::rWishart(1, df, Sigma)[ , , 1]
  atr <- attributes(x)
  attributes(x) <- c(atr, n = df + 1)
  if(covariance == TRUE){
    x <- x / df
    class(x) <- c("covariance", "matrix")
    x
  }
  x
}

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
