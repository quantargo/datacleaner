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
#' windsorize(c(92, 19, 101, 58, 101, 91, 26, 78, 10, 13, −5, 101, 86, −5))
#' @export
windsorize <- function(x, p = .90) {
  q <- quantile(x, p)
  x[x >= q] <- q
  x
}

