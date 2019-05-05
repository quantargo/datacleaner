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