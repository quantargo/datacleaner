transform_log <- function(x) {
  if (x<0) {stop("input can't be negative")}
  x <- log(x)
  x
}