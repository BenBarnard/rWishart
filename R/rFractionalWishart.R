#' Random Fractional Wishart Matrix
#'
#' @inherit rWishart
#' @export
#'
#' @examples rFractionalWishart(2, 22.5, diag(1, 20))
rFractionalWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  replicate(n, rWishart::FractionalWishart(df, Sigma, covariance),
            simplify = simplify)
}



#' Fractional Wishart Helper Function
#'
#' @inherit rWishart
#' @export
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @examples FractionalWishart(22.5, diag(1, 20))
FractionalWishart <- function(df, Sigma, covariance = FALSE){
  if(ncol(Sigma) > df){stop("Cannot produce a Singular Fractional Wishart")}
  cholesky <- chol(Sigma)
  B <- matrix(0, ncol = ncol(Sigma), nrow = ncol(Sigma))
  for(i in 1:ncol(Sigma)){
    for(j in 1:ncol(Sigma)){
      B[i, j] <- ifelse(j < i, rnorm(1), 0)
    }
    B[i, i] <- rgamma(1, df - i + 1, scale = 1 / 2)
  }
  x <- cholesky %*% t(B) %*% B %*% t(cholesky)
  atr <- attributes(x)
  attributes(x) <- c(atr, n = df + 1)
  if(covariance == TRUE){
    x <- x / df
    class(x) <- c("covariance", "matrix")
    x
  }
  x
}