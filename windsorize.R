#' Windsorize
#'
#' Do some windsorization.
#' @export
windsorize <- function(x, p = .90) {
  if (length(x) == 0) stop("Not allowed the use of an empty vector!")
  if (all(is.na(x)))  stop("Not allowed the use of a vector containing only NA!")
  
  q <- quantile(x, probs=c(1-p,p))
  x[x >= q] <- q
  x
}

