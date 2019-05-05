#' transform_log
#'
#' log-transformation of a numeric vector. For details about log-transformation please see you basic school math textbook.
#' @param x a numeric vector
#' @return log-transformed vector \code{x}
#' @examples
#' example_vector=c(1,2,3,4,5,6,7,8,9,10)
#' transform_log(example_vector)
#' @export

transform_log <- function(x){
  
  if( is.null(x) )    stop("Input vector is not allowed to be NULL.")
  if( any(is.na(x)) ) stop("There is at least one NA value in input vector.")
  if( any(x <= 0)  )  stop("There is at least one negative value.")
  if( any(is.numeric(x) == FALSE) )  stop("There is at least one non-numeric value.")
  y<-log(x)
  return(y)
  
}