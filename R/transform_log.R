#' Transform_log
#' 
#' Log transformation.
#' 
#' @param x A vector.
#' 
#' @examples
#' transform_log(exp(rnorm(2)))
#' 
#' @export

transform_log <- function(x) {
  if (any(x<0)) {stop("input can't be negative")}
  x <- log(x)
  x
}