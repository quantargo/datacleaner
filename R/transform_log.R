#' transform_log 
#' Transform numerical values into their log values
#' @param x A vector
#' @return logarithm of x
#' @examples
#' transform_log(c(NA,0,-1,exp(2)))
#'@export 
transform_log<-function(x){
  if(!is.numeric(x))stop("function is expecting only numeric values")
  x_nan<-is.na(x)
  x[x_nan]<-1
  ifelse(x<0,"OK", warning("input vector contains negative values, turned into NA"))
  y<-log(x[x>=0])
  x[x>=0]<-y
  x[x<0]<-NA
  x[x_nan]<-NA
  x
}