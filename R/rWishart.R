#' Random Wishart Distributed Matrices
#'
#' @inheritParams base::replicate
#' @param df degrees of freedom
#' @param Sigma Covariance matrix
#' @param covariance should covariance matrix be generated (X / df)
#'
#' @return list or array of random wishart matrices
#' @export
#'
#' @examples rWishart(2, 5, diag(1, 20))
rWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  if(df >= ncol(Sigma)){
    ls <- rNonsingularWishart(n, df, Sigma, covariance, simplify)
  }else{
    ls <- rPsuedoWishart(n, df, Sigma, covariance, simplify)
  }
  ls
}


