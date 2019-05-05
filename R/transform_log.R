#' Log transforms data.
#'
#' @param x A numeric vector containing the data
#' @return A numeric vector whose entries are the logarithms of the input.
#' @examples
#' log(1:5)
#' @export
transform_log<-function(x){
  if(length(x)==0){
    stop("x must have positive length")
  }else if(sum(x<=0)>0){
    stop("x must not contain non-positive values")
  }else if(sum(is.na(x))>0){
    stop("x must not contain any NA")
  } 
  log(x)
} 