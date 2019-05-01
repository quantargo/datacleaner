#' Windsorize
#'
#' Do some windsorization.
#' @export
windsorize <- function(x, p = .90) {
  if(all(is.na(x))) {stop("argument should not be a vector containing only NA-s")}
  if(all(is.null(x))) {stop("argument should not be a null vector")}
  q_up <- quantile(x, 0.5 + p / 2 )
  q_down <- quantile(x, 0.5 - p / 2 )
  x[x >= q_up] <- q_up
  x[x <= q_down] <- q_down
  x
}

