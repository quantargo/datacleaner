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
  if (any(x<0)) {stop("log values must be positive")}
  x <- log(x)
  x
}