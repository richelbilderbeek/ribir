#' Checks if x is a single, whole-number variable
#' @param x the number to check
#' @param tolerance the maximum error a number may deviate from a whole number,
#'   before it is labeled as a floating point value
#' @return TRUE or FALSE
#' @author Richel Bilderbeek
is_whole_number <- function(
  x,
  tolerance = .Machine$double.eps ^ 0.5
) {
  if (length(x) > 1) return(FALSE)
  if (!is.numeric(x)) return(FALSE)
  return(abs(x - round(x)) < tolerance)
}
