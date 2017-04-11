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
                    rWishart::WishFunc(df, Sigma, covariance),
                    simplify = simplify)
  }else{
    cholesky <- chol(Sigma)
    ls <- replicate(n,
                    rWishart::SingularWishFunc(df, Sigma, cholesky, covariance),
                    simplify = simplify)
  }
  ls
}


