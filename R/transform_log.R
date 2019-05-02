#' transform_log 
#' Transform numerical values into their log values
#' @param x A vector
#' @return logarithm of x
#' @export 
#' @examples
#' transform_log(1)
#' transform_log(c(1, 2, 3, 4, 5))
#' 
transform_log <- function(x) { 
  if (!is.numeric(x)) {
    warning("transform_log: Expecting numeric argument")
  }
  x_badval <- is.na(suppressWarnings(as.numeric(x)))
  x[x_badval] <- 1
  y <- log(as.numeric(x))
  y[x_badval] <- NA
  y
} 