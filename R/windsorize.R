#' Windsorize
#'
#' Do some windsorization.
#' @export
windsorize <- function(x, p = .90) {
  q <- quantile(x, probs = c(1-p,p))
  x[x >= q] <- q
  x
}

