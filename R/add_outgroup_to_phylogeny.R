#' Adds an outgroup to a phylogeny
#' @param phylogeny a phylogeny
#' @param stem_length How long will the length of the outgroup be before
#'   the crown?
#' @param outgroup_name How is the outgroup named?
#' @return A phylogeny, of type phylo
#' @export
#' @author Richèl Bilderbeek
add_outgroup_to_phylogeny <- function(
  phylogeny,
  stem_length,
  outgroup_name = "Outgroup"
) {

  if (!is_phylogeny(phylogeny)) {
    stop("phylogeny must be a phylogeny")
  }
  if (class(stem_length) != "numeric") {
    stop("stem_length must be a number")
  }
  if (class(outgroup_name) != "character") {
    stop("outgroup_name must be a word")
  }

  n_taxa <- length(phylogeny$tip.label)

  crown_age <- ape::dist.nodes(phylogeny)[n_taxa + 1][1] # nolint ape does not use snake_case
  phylogeny$root.edge <- stem_length # nolint ape does not use snake_case
  # Add an outgroup
  # Thanks to Liam J. Revell,
  # http://grokbase.com/t/r/r-sig-phylo/12bfqfb93a/adding-a-branch-to-a-tree
  tip <- list(
    edge = matrix(c(2, 1), 1, 2),
    tip.label = outgroup_name,
    edge.length = crown_age + stem_length,
    Nnode = 1
  )
  class(tip) <- "phylo"
  # Attach to any node, in this case to the root. Note: order matters
  phylogeny <- ape::bind.tree(tip, phylogeny) # nolint ape does not use snake_case

  return(phylogeny)
}
