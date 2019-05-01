#' Windsorize
#'
#' Do some windsorization.
#' @export
windsorize <- function(x, p = .90) {
  if(length(x) == 0) stop('Empty vector passed as an argument.')
  if( sum(is.na(x)) == length(x) )  stop('A vector of NA-s passed as an argument')
  q <- quantile(x, probs = c(1-p,p), na.rm = TRUE)
  x[x >= q[2] ] <- q[2]
  x[x <= q[1] ] <- q[1]
  x
}

