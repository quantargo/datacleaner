transform_log <- function(x){
  
  if( sum(is.na(x)) ){
    stop("Input x contains value NA")
  }
  else if( length(x) ){
    stop("Input x is NULL")
  }  
  else if( sum(x <= 0)  ){
    stop("Input x contains non-positive values")
  }  
  
  log(x)
  
}
