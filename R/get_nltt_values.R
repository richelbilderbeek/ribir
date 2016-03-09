#' Collects the nLTT values of all phylogenies in the melted/uncast/long form
#'
#' @param phylogenies the phylogenies, supplied as either a list or a multiPhylo object, where the phylogenies are of type 'phylo'
#' @param dt The timestep resolution, where 1/dt is the number of points evaluated
#' @return A dataframe of timepoints with the nLTT value of each phylogeny in time
#'
#' @export
get_nltt_values <- function(phylogenies, dt) {
  m <- get_nltt_values_unmelted(phylogenies = phylogenies, dt = dt)
  reshape2::melt(m, id = "time")
}


#' Collects the nLTT values of all phylogenies in the unmelted/cast/short form
#'
#' @param phylogenies the phylogenies, supplied as either a list or a multiPhylo object, where the phylogenies are of type 'phylo'
#' @param dt The timestep resolution, where 1/dt is the number of points evaluated
#' @return A dataframe of timepoints with the nLTT value of each phylogeny in time
#'
#' @export
get_nltt_values_unmelted <- function(phylogenies, dt) {
  if (length(phylogenies) < 1) {
    stop("get_average_nltt_matrix: ",
         "there must be at least one phylogeny supplied")
  }
  if (class(phylogenies) != "multiPhylo" && class(phylogenies) != "list") {
    stop("get_average_nltt_matrix: ",
         "phylogenies must be of class 'multiPhylo' or 'list', ",
         "used '", class(phylogenies), "' instead")
  }
  if (!inherits(phylogenies[[1]], "phylo")) {
    # Stop imposed by ape::ltt.plot.coords
    stop("get_average_nltt_matrix: ",
         "phylogenies must be of type phylo, ",
         "instead of '", class(phylogenies[[1]]), "'")
  }
  if (dt <= 0.0 || dt >= 1.0) {
    stop("get_average_nltt_matrix: ",
         "dt must be between (not including) zero and one, ",
         "dt was ", dt, " instead")
  }

  n_rows <- (1.0 / dt) + 1
  n_phylogenies <- length(phylogenies)
  n_cols <- n_phylogenies + 1
  m <- matrix(nrow = n_rows, ncol = n_cols)
  m[, 1] <- seq(0,1,dt)
  for (i in seq(1, n_phylogenies)) {
    testit::assert(i >= 1 && i <= n_phylogenies)
    new_col <- ribir::stretch_nltt_matrix(
      m = ribir::get_phylogeny_nltt_matrix(phylogenies[[i]]),
      dt = dt,
      step_type = "upper"
    )
    testit::assert(length(new_col[, 2]) == n_rows)
    m[, i + 1]  <- new_col[, 2, drop = FALSE]
  }
  z <- as.data.frame(x = m)
  colnames(z) <- c("time", paste(rep("N", 2),
    as.character(seq(1, n_phylogenies)), sep = ""))
  z
}
