#' Windsorizes data.
#'
#' @param x A numeric vector containing the data.
#' @param p A numeric value denoting the a probability
#' @return A numeric vector containing the windsorized data, i.e. extreme values are replaced by the quantiles derived from \code{p}.
#' @examples
#' windsorize(c(92 , 19 , 101 , 58 , 1053 , 91 , 26 , 78 , 10 , 13 , -40 , 101 , 86 , 85 , 15 , 89 , 89 , 28 , -5 , 41))
#' @export
windsorize <- function(x, p = .90) {
  if(length(x)==0){
    stop("x must be of positive length")
  }else if(sum(is.na(x))>0){
    stop("x must not contain any NA")
  }
  q <- quantile(x, 0.5 * ( 1 + p * c(-1,1) ) )
  x[x >= q[2] ] <- q[2]
  x[x <= q[1] ] <- q[1] 
  x
}

