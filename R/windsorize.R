#' Windsorize
#'
#'
#' Transform all outliner data to
#' (1-p)/2 percentile value for lower outliers and 
#' (1+p)/2 for higher outliers.
#' 
#' @param x A vector.
#' @param p A quantile.
#' @return inuput vector x with trimmed outliers by (1-p) percentile.
#' @examples 
#' windsorize(rnorm(100,0,1))
#' @export
windsorize <- function(x, p = .90) {
  if(is.null(x))stop("vector is empty")
  y<-x[!is.na(x)]
  if(length(y)==0)stop("vector contains only NAs")
  q_max <- quantile(y, (1+p)/2)
  q_min<- quantile(y,(1-p)/2)
  x[x >= q_max] <- q_max
  x[x<= q_min]<-q_min
  x
}