#' Windsorize
#'
#' Do some windsorization.
#' @param x A numerical vector.
#' @param p Quantile value for outliers removal
#' @examples  
#' windsorize(c(5,10,15,50))
#' @export
windsorize <- function(x, p = .90) {
  if (length(x) == 0)
    { stop("Not allowed the use of an empty vector!")}
  if (all(is.na(x))) 
    { stop("Not allowed the use of a vector containing only NA!")}

  q <- quantile(x, probs=c(1-p,p) , na.rm = TRUE)
  x[x >= q[2] ] <- q[2]
  x[x <= q[1] ] <- q[1]
  x
}

