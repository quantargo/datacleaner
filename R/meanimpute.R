#' Meanimputation
#'
#' @param x A numeric vector containing the data
#' @return the input vector with its NA values replaced by the mean
#' @examples
#' log(1:5)
#' @export
meanimpute <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}
