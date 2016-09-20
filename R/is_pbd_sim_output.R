#' Does not use pbd_sim()$stree, but generates these like PBD does
#' @param pbd_sim_output the argument tested to output of PBD::pbd_sim
#' @param verbose will the function show why the input
#'   is determined not to be  output of PBD::pbd_sim
#' @return TRUE or FALSE
#' @export
#' @author Richel Bilderbeek
is_pbd_sim_output <- function(
  pbd_sim_output,
  verbose = FALSE
) {
  if (typeof(pbd_sim_output) != "list") {
    if (verbose) {
      message("is_pbd_sim_output:",
        "typeof(pbd_sim_output) != \"list\", typeof(pbd_sim_output) is ",
        typeof(pbd_sim_output)
      )
    }
    return(FALSE)
  }
  if (length(pbd_sim_output) < 9) {
    if (verbose) {
      message(
        "is_pbd_sim_output:",
        "length(pbd_sim_output) < 9, was",
        length(pbd_sim_output)
      )
    }
    return(FALSE)
  }
  if (class(pbd_sim_output$tree) != "phylo") {
    if (verbose) {
      message(
        "is_pbd_sim_output:",
        "class(pbd_sim_output$tree) != \"phylo\"",
        "class(pbd_sim_output$tree) is",
        class(pbd_sim_output$tree)
      )
    }
    return(FALSE)
  }
  return(TRUE)
}
