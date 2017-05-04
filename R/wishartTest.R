#' Test if Matrix is a Wishart Matrix
#'
#' @param WishMat Wishart Matrix
#' @param Sigma Covariance matrix
#' @param vec random vector
#'
#' @export
#' @importFrom stats rnorm
#'
#' @examples wishartTest(rWishart(1, 5, diag(1, 20), simplify = FALSE)[[1]], diag(1, 20))
wishartTest <- function(WishMat, Sigma, vec = NULL){
  if(is.null(vec)){
    vec <- rnorm(ncol(WishMat))
  }
  (t(vec) %*% WishMat %*% vec) / 
           (t(vec) %*% Sigma %*% vec)
}