#' Checks if the input is a phylogeny
#' @param x input to be checked
#' @return TRUE or FALSE
#' @author Richel Bilderbeek
#' @export
is_phylogeny <- function(x) {
  # Is x a single phylogeny?
  return (class(x) == "phylo")
}
