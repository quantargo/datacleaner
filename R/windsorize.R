#' Windsorize
#'
#' Do some windsorization.
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

