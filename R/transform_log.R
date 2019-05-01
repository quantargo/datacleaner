#' Log Transform
#'  
#' Transform numerical values into their log values.
#' @param x A numeric vector.
#' @return The log values of \code{x}.
#' @examples
#' transform_log(exp(rnorm(7)))
#' @export
 
transform_log<- function( x )
{
  if( !is.numeric(x)) stop('Non numeric values found in passed paramenter.')
  log(x )
}