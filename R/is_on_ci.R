#' Determines if the environment is AppVeyor
#' @return TRUE if run on AppVeyor, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_on_appveyor <- function() {
  Sys.getenv("APPVEYOR") != ""
}

#' Determines if the environment is Travis CI
#' @return TRUE if run on Travis CI, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_on_travis <- function() {
  Sys.getenv("TRAVIS") != ""
}

#' Determines if the environment is Travis CI
#' @return TRUE if run on Travis CI, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_on_ci <- function() {
  is_on_appveyor() || is_on_travis()
}
