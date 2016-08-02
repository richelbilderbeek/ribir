#' Sees if the values of a distribution are normally distributed
#'  using a Shapiro-Wilk normality test
#' @param values the values to check
#' @param p_value the p value
#' @return TRUE when it cannot be rejected that the distribution is normal,
#'    or FALSE when it can be rejected that the distribution is normal
#' @examples
#'    # Create a normal disribution
#'    nd <- rnorm(n = 1000, mean = 0.0, sd = 1.0)
#'    testit::assert(is_distributed_normally(nd))
#'
#'    # Create a non-normal disribution
#'    nnd <- runif(n = 1000, min = 0.0, max = 1.0)
#'    testit::assert(!is_distributed_normally(nnd))
#' @export
#' @author Richel Bilderbeek
is_distributed_normally <- function(
  values,
  p_value = 0.05
) {
  if (any(is.na(values)) || any(!is.numeric(values))) {
    stop("is_distributed_normally: all values must be numeric")
  }
  if (length(values) < 3L || length(values) > 5000L) {
    stop("is_distributed_normally: sample size must be between 3 and 5000")
  }
  t <- shapiro.test(x = values)
  if (t$p >= p_value) {
    # Cannot reject distribution is normal
    return (TRUE)
  }
  if (t$p < 0.05) {
    # Reject distribution is normal
    return (FALSE)
  }
}
