#' Meanimputation
#' Calculates mean of a given vector, ignores NA values.
#' @param x A vector
#' @export
meanimpute <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}
