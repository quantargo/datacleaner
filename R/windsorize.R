#' Windsorize
#'
#' Do some windsorization.
#' @export
windsorize <- function(x, p = .90) {
  q <- quantile(x, p)
  x[x >= q] <- q
  x
}

