#' Get the average nLTT from a collection of phylogenies
#'
#' @param phylogenies the phylogenies, where the phylogenies are of type 'phylo'
#' @param dt The timestep resolution, where 1/dt is the number of points evaluated
#' @param plot_nltts Also plot each nLLT line
#' @param xlab Label on the x axis
#' @param ylab Label on the y axis
#' @param replot If false, start a clean plot. If true, plot the new data over the current
#' @param ... Plotting options
#' @return Nothing
#' @examples
#'   get_average_nltt(c(ape::rcoal(10),ape::rcoal(10)))
#'   get_average_nltt(c(ape::rcoal(10),ape::rcoal(20)), dt = 0.1)
#'
#' @export
get_average_nltt <- function(
  phylogenies,
  dt = 0.001,
  plot_nltts = FALSE,
  xlab = "Normalized Time",
  ylab = "Normalized Lineages",
  replot = FALSE,
  ...
) {
  if (class(phylogenies) != "multiPhylo") {
    warning("get_average_nltt: phylogenies must be of class 'multiPhylo', used '",class(phylogenies),"' instead")
  }
  if (dt <= 0.0 || dt >= 1.0) {
    stop("get_average_nltt: dt must be between (not including) zero and one, dt was ",dt," instead")
  }


  xy <- ribir::get_average_nltt_matrix(phylogenies = phylogenies, dt = dt)

  # Set the shape of the plot
  if (replot == FALSE) {
    plot.default(
      xy,
      xlab = "Normalized Time",
      ylab = "Normalized Lineages",
      xaxs = "r",
      yaxs = "r",
      type = "S",
      xlim=c(0,1),
      ylim=c(0,1),
      ...
    )
  }

  # Draw the nLTTS plots used
  if (plot_nltts == TRUE) {
    # Copied
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
    # End of copy

    for (stretch_matrix in stretch_matrices) {
      lines.default(
        stretch_matrix,
        xaxs = "r",
        yaxs = "r",
        type = "S",
        col="grey",
        xlim=c(0,1),
        ylim=c(0,1)
      )
    }
  }

  # Redraw the average nLTT plot
  lines.default(
    xy,
    xaxs = "r",
    yaxs = "r",
    type = "S",
    xlim=c(0,1),
    ylim=c(0,1),
    ...
  )
}
