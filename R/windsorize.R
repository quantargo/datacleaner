#' Windsorize
#'
#' Do some windsorization.
#' @export
windsorize <- function(x, p = .90) {
  
  if( sum(is.na(x))){
    stop("Input x contains value NA")
  }
  else if( is.null(x) ){
    stop("Input x is NULL")
  }
  
  q_lower <- quantile(x, (1-p)/2)
  q_upper <- quantile(x, p + (1-p)/2)
  x[x <= q_lower] <- q_lower
  x[x >= q_upper] <- q_upper
  x
}

