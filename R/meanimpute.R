#' Meanimputation
#' 
#' Removes NA-s with the mean value for the vector \code{x}
#' 
#' @param x A numeric vector
#' @export
meanimpute <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}
