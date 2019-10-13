#' Checks if the input is a phylogeny
#' @param phylogeny input to be checked
#' @return TRUE or FALSE
#' @author Richel Bilderbeek
#' @export
is_phylogeny <- function(phylogeny) {
  is_phylogeny <- FALSE
  tryCatch({
    ribir::check_phylogeny(phylogeny)
    is_phylogeny <- TRUE
  }, error = function(e) {} # nolint indeed no use of e, empty function indeed
  )
  is_phylogeny
}
