#' Windsorize
#'
#' Winsorizing or winsorization is the transformation of statistics by limiting 
#' extreme values to reduce the effect of 
#' spurious outliers in statistical data.
#' 
#' @param x A vector.
#' @param p A quantile.
#' 
#' @examples 
#' windsorize(rnorm(5))
#' 
#' @export
windsorize <- function(x, p = .90) {
  if (is.null(x)) {
    stop("nul vector")}
    if(is.na(x)){
      stop("NA vector")}
  q_low <- quantile(x, 0.5-p/2)
  q_high <- quantile(x, 0.5+p/2)
  x[x >= q_high] <- q_high
  x[x <= q_low] <- q_low
  x
}

