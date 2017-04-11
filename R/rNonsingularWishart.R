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