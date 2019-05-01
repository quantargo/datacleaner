#' Windsorize
#' 
#' Winsorizing or winsorization is the transformation of statistics by limiting
#' extreme values in the statistical data to reduce the effect of possibly
#' spurious outliers.
#' 
#' @param x A vector.
#' @param p A quantile.
#' 
#' @examples
#' windsorize(rnorm(5))
#' 
#' @export
windsorize <- function(x, p = .90) {
  if(all(is.na(x))) {stop("argument should not be a vector containing only NA-s or NULL-s")}
  q_up <- quantile(x, 0.5 + p / 2 )
  q_down <- quantile(x, 0.5 - p / 2 )
  x[x >= q_up] <- q_up
  x[x <= q_down] <- q_down
  x
}

