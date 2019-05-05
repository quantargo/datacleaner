#' Windsorize
#'
#' Its purposes is to eliminate outliers in a following way. Values of (0.5 +- p/2)th quantiles are calculated and all
#' values above(below) those quantiles are replaced by the quantiles.
#' @param x a numeric vector
#' @param p quantile
#' @return Windsorized  vector \code{x}
#' @examples
#' example_vector=c(-1000,1,2,3,4,5,6,7,8,9,1000)
#' windsorize(example_vector, 0.9)
#' 
#' example_vector=rnorm(100)
#' windsorize(example_vector, 0.9)
#' @export
#' @import stats

windsorize <- function(x, p = .90) {
  
  if(is.null(x)) {stop("Input vector cannot be NULL.")}
  if(any(is.na(x))) {stop("There should be no NA's in input vector.")}
  if(all(is.numeric(x)==FALSE)) {stop("There should only numeric values in the input vector.")}
  
  if(is.na(p)==TRUE) {stop("Input quantile should be a number between 0 and 1 ")}
  if(is.numeric(p)==FALSE) {stop("Input quantile should be a number between 0 and 1 ")}
  if(p > 1) {stop("Input quantile should be a number between 0 and 1 ")}
  if(p < 0) {stop("Input quantile should be a number between 0 and 1 ")}
  
  
  q_u <- quantile(x, 0.5 + p/2)
  x[x >= q_u] <- q_u
  
  q_l <- quantile(x, 0.5 - p/2)
  x[x <= q_l] <- q_l
  
  return(x)
}
