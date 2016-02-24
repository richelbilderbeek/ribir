#' Get the average nLTT from a collection of phylogenies
#'
#' @param phylogenies the phylogenies, where the phylogenies are of type 'phylo'
#' @param dt The timestep resolution, where 1/dt is the number of points evaluated
#' @return A matrix of timepoints with the average number of (normalized) lineages through (normalized) time
#' @examples
#'   get_average_nltt(c(ape::rcoal(10),ape::rcoal(20)))
#'
#' @export
get_average_nltt_matrix <- function(
  phylogenies,
  dt = 0.001) {

  if (dt <= 0.0 || dt >= 1.0) {
    stop("get_average_nltt_matrix: dt must be between (not including) zero and one, dt was ",dt," instead")
  }

  sz <- length(phylogenies)

  nltts <- NULL
  for (phylogeny in phylogenies) {
    nltts <- c(nltts,list(get_phylogeny_nltt_matrix(phylogeny)))
  }
  testit::assert(length(nltts) == length(phylogenies))

  stretch_matrices <- NULL
  for (nltt in nltts) {
    stretch_matrix <- stretch_nltt_matrix(nltt,dt = dt, step_type = "upper")
    stretch_matrices <- c(stretch_matrices,list(stretch_matrix))
  }
  testit::assert(length(stretch_matrices) == length(nltts))

  xy <- stretch_matrices[[1]]
  for (i in seq(2,sz)) {
    xy <- (xy + stretch_matrices[[i]])
  }
  xy <- (xy / sz)

  xy
}
