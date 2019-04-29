#' Windsorize
#'
#' Do some windsorization.
#' 
#' @param x A numeric vector containing real numbers
#' @param p A number between 0 and 1, representing the mass of probability. Values are winsorized outside \code{p}. Default value is 0.9. 
#' @return Windsorized vector of \code{x}.
#' @examples
#' windsorize(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' windsorize(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), p = 0.8)
#' windsorize(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 0.8)
#' @export

windsorize <- function(x, p = .90) {
  
  if( sum(is.na(x))){
    stop("Input x contains value NA")
  }
  else if( length(x) == 0 ){
    stop("Input x is NULL")
  }
  
  q_lower <- quantile(x, (1-p)/2)
  q_upper <- quantile(x, p + (1-p)/2)
  x[x <= q_lower] <- q_lower
  x[x >= q_upper] <- q_upper
  x
}

