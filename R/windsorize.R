#' Windsorize
#'
#' Replacing values of vector \code{x} greater or smaller then \code{q} quantile values.
#' 
#' @param x A numerical vector.
#' @param p Quantile value for outliers removal.
#' 
#' @examples  
#' windsorize(c(1,499,500,501))
#' @export
#' 
windsorize <- function(x, p = .90) {
  if(length(x) == 0) stop('Empty vector passed as an argument.')
  if( sum(is.na(x)) == length(x) )  stop('A vector of NA-s passed as an argument')
  q <- quantile(x, probs = c(1-p,p), na.rm = TRUE)
  x[x >= q[2] ] <- q[2]
  x[x <= q[1] ] <- q[1]
  x
}

