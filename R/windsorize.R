#' Windsorize
#'
#' Do some windsorization.
#' @export
windsorize <- function(x, p = .90) {
  q_lower <- quantile(x, (1-p)/2)
  q_upper <- quantile(x, p + (1-p)/2)
  x[x <= q_lower] <- q_lower
  x[x >= q_upper] <- q_upper
  x
}

