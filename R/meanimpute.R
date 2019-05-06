#' Meanimputation
#' @param x A vector
#' @export
meanimpute <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}
