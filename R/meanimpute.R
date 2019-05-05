#' Meanimpute
#'
#' Replace NA's with mean value
#' @param x a numeric vector
#' @return new vector, where NA's are replace by mean \code{x}
#' @examples
#' example_vector=c(1,5,NA,NA)
#' meanimpute(example_vector)
#' @export
meanimpute <- function(x) {
  
  if(is.null(x)) {stop("Input vector cannot be NULL.")}
  if(all(is.na(x))) {stop("Input vector should contain at least one numeric element.")}
  if(any(is.numeric(x)==FALSE)) {stop("Input vector should contain at least one numeric element.")}
  
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}
