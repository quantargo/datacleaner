#' Windsorize
#'
#' set all outliers to a specified percentile of the data; 
#' a 90% winsorization would see all data below the 5th percentile set 
#' to the 5th percentile, 
#' and data above the 95th percentile set to the 95th percentile.
#' 
#' @param x A vector.
#' @param p A quantile.
#' @return dataset with trimmed outliers with 10% percentile
#' @examples
#' windsorize(c(3,4,4,3,4,5,1))
#' @export
windsorize <- function(x, p = .90) {
  if (length(x) == 0) stop("argument should not be a empty vector")
  if (all(is.na(x))) {
    stop("argument should not be a vector containing only NA")
  }
  if (p < 0 || p > 1) {
    stop("p invalid percentale. Expected values from 0 to 1")
  }
  q_lower <- quantile(x, (1-p)/2)
  q_upper <- quantile(x, 1 - (1-p)/2)
  x[x <= q_lower] <- q_lower
  x[x >= q_upper] <- q_upper
  x
}

