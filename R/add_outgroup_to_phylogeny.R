#' Adds an outgroup to a phylogeny
#' @param phylogeny a phylogeny
#' @param stem_length How long will the length of the outgroup be before the crown?
#' @param outgroup_name How is the outgroup named?
#' @return A phylogeny, of type phylo
#' @export
#' @author Richel Bilderbeek
add_outgroup_to_phylogeny <- function(
  phylogeny,
  stem_length,
  outgroup_name = "Outgroup"
) {

  if (class(phylogeny) != "phylo") {
    stop(
      "add_outgroup_to_phylogeny: ",
      "parameter 'phylogeny' must be of type 'phylo', ",
      "instead of type", class(phylogeny)
    )
  }
  if (class(stem_length) != "numeric") {
    stop(
      "add_outgroup_to_phylogeny: ",
      "parameter 'stem_length' must be of type 'numeric', ",
      "instead of type", class(stem_length)
    )
  }
  if (class(outgroup_name) != "character") {
    stop(
      "add_outgroup_to_phylogeny: ",
      "parameter 'outgroup_name' must be of type 'character', ",
      "instead of type", class(outgroup_name)
    )
  }

  n_taxa <- length(phylogeny$tip.label)

  crown_age <- ape::dist.nodes(phylogeny)[n_taxa + 1][1]
  phylogeny$root.edge <- stem_length
  # Add an outgroup
  # Thanks to Liam J. Revell,
  # http://grokbase.com/t/r/r-sig-phylo/12bfqfb93a/adding-a-branch-to-a-tree
  tip <- list(
    edge = matrix(c(2, 1), 1, 2),
    tip.label = "Outgroup",
    edge.length = crown_age + stem_length,
    Nnode = 1
  )
  class(tip) <- "phylo"
  # Attach to any node, in this case to the root. Note: order matters
  phylogeny <- ape::bind.tree(tip, phylogeny)

  return(phylogeny)
}
