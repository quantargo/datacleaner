transform_log<- function(x){
  if (x<0) stop("Negative input not allowed")
  if (x==0) stop("Input 0 not allowed")
  if (!is.numeric(x)) stop("Not numeric input not allowed")
  if (is.null(x)) stop("NULL input not allowed")
  
  log(x)
}
