#' Checks if the input is a phylogeny
#'
#' Will stop if not, will do nothing otherwise
#' @param phylogeny object to be checked
#' @author Richèl J.C. Bilderbeek
#' @export
check_phylogeny <- function(phylogeny) {
  if (class(phylogeny) != "phylo") {
    stop(
      "'phylogeny' must be of class 'phylo'. \n",
      "Actual class: ", class(phylogeny)
    )
  }
}
