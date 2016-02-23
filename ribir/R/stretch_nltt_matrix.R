# Stretch matrix 'm' with a timestep resolution of 'dt'

#' Stretch a matrix
#'
#' @param m A matrix of 2 columns and at least 2 rows
#' @param dt The resultion, a value e <0,1]
#' @param step_type can be 'lower' or 'upper
#' @return The stretched matrix
#' @examples
#'   m <- matrix( c(c(0.0,1.0), c(0.5,1.0)), ncol = 2, nrow = 2)
#'   expected <- matrix(
#'     c(
#'       c(0.0,0.5,1.0),  # Timepoints
#'       c(0.5,0.5,1.0)   # Values
#'     ),
#'     ncol = 2, nrow = 3
#'   )
#'   result <- stretch_nltt_matrix(m = test, dt = 1.0/3)
#'   assert(identical(result,expected))
#'
#' @export
stretch_nltt_matrix <- function(
  m,
  dt,
  step_type = "lower"
) {
  # Stretch matrix 'm' with a timestep resolution of 'dt'
  assert(is.matrix(m))
  assert(ncol(m) == 2)
  assert(nrow(m) >= 2)
  assert(step_type == "lower" || step_type == "upper")

  # Prepare a new matrix called n
  n_nrow <- 1 + (1 / dt)
  n_ts <- seq(0.0,1.0,dt)
  n_ns <- rep(NA,times = n_nrow)
  n <- matrix( c(n_ts,n_ns), ncol = 2, nrow = n_nrow)
  names(n) <- names(m)

  # Fill in the N's in n
  m_row_index <- 1
  for (n_row_index in seq(1,n_nrow)) {
    if (n[n_row_index,1] >= m[m_row_index + 1,1]) {
      m_row_index <- m_row_index + 1
      #print("New m_row_index: ")
      #print(m_row_index)
    }
    n[n_row_index,2] <- m[m_row_index + ifelse(step_type == "lower",0,1) ,2]
  }
  n
}



