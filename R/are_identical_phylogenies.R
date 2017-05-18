#' Determines if two phylogenies are equal
#' as parsed by olli's rBEAST package its function beast2out.read.trees
#' @param p the first phylogeny
#' @param q the second phylogeny
#' @return TRUE or FALSE
#' @examples
#'   p <- ape::rcoal(10)
#'   q <- ape::rcoal(10)
#'   testit::assert(are_identical_phylogenies(p, p))
#'   testit::assert(!are_identical_phylogenies(p, q))
#' @author Richel Bilderbeek
#' @export
are_identical_phylogenies <- function(p, q) {
  if (!is_phylogeny(p)) {
    stop("p must be a phylogeny")
  }
  if (!is_phylogeny(q)) {
    stop("q must be a phylogeny")
  }
  return(ape::all.equal.phylo(p, q))
}
