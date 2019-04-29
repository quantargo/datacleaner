#' Meanimputation
#'
#' Replaces NA values with the mean of \code{x}
#' 
#' @param x A numeric vector containing real numbers
#' @return Imputed vector \code{x} where NAs are replaced with the mean of \code{x}
#' @examples
#' meanimpute(x = c(1, NA, 3, 4, 5, 6, 7, 8, NA, 10))
#' meanimpute(c(1, NA, 3, 4, 5, 6, 7, 8, NA, 10))
#' @export

meanimpute <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}
