#' Random Nonsingular Wishart Matrix
#'
#' @inherit rWishart
#' @export
#'
#' @examples rNonsingularWishart(2, 20, diag(1, 5))
rNonsingularWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  replicate(n,
            rWishart::NonsingularWishart(df, Sigma, covariance),
            simplify = simplify)
}


#' Nonsingular Wishart Helper Function
#'
#' @inherit rWishart
#' 
#' @export
#' @keywords internal
#' @importFrom stats rWishart
#'
#' @examples NonsingularWishart(20, diag(1,5))
NonsingularWishart <- function(df, Sigma, covariance = FALSE){
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