#' Log Transform
#'  
#' Transform numerical values into their log values.
#' @export
 
transform_log<- function( x )
{
  if( !is.numeric(x)) stop('Non numeric values found in passed paramenter.')
  log(x )
}