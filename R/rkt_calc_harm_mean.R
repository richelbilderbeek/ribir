#' Calculate the harmonic mean of a numeric vector
#' @param x a numeric vector
#' @return the harmonic mean of vector \code{x}
#' @author Richel J.C. Bilderbeek
#' @export
rkt_calc_harm_mean <- function(x) {
  1.0 / mean(1.0 / x)
}
