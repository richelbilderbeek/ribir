#' Sends it error to cat
#' @param i index
#' @export
cat_debug_info <-function(i) {
  cat(traceback(), file = "cat_debug_info.log")
  stop("Fail at i equals", i)
}
