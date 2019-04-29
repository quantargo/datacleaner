#' Transform_log
#'
#' Transforms values to natural logarithm.
#' 
#' @param x A numeric vector containing positive real numbers
#' @return Natural logarithm of the values in \code{x}.
#' @examples
#' transform_log(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' transform_log(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' @export

transform_log <- function(x){
  
  if( sum(is.na(x)) ){
    stop("Input x contains value NA")
  }
  else if( length(x) == 0 ){
    stop("Input x is NULL")
  }  
  else if( sum(x <= 0)  ){
    stop("Input x contains non-positive values")
  }  
  
  log(x)
  
}
