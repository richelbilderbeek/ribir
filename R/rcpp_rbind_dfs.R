#' Creates a data frame with three columns:
#' (1) filename, a string, (2) replicate, a factor,
#' (3) value, a double. The number of rows is between
#' one and ten
create_random_df <- function() {
  n_rows <- sample(1:10, 1)

  df <- data.frame(
    filename = rep(paste(sample(LETTERS, 8), collapse = ""), n_rows),
    replicate = as.factor(rep(sample(1:100, 1), n_rows)),
    value = runif(n_rows),
    stringsAsFactors = FALSE
  )
  testit::assert(is.character(df$filename))
  testit::assert(is.factor(df$replicate))
  testit::assert(is.numeric(df$value))
  return (df)
}

#' Creates 100 data frames with three columns:
#' (1) filename, a string, (2) replicate, a factor,
#' (3) value, a double. The number of rows for each
#' data frame is between
#' one and ten
create_random_dfs <- function(n = 100) {
  dfs <- list(n)
  for (i in seq(1,n))
  {
    dfs[[i]] <- create_random_df()
  }
  testit::assert(is.list(dfs))
  testit::assert(is.data.frame(dfs[[1]]))
  return (dfs)
}

#' Combines multiple random-row data frames using rbind.
#' It is to be compared to rcpp_rbind_dfs, that does the
#' same using Rcpp
#' @author Richel Bilderbeek
#' @export
rbind_dfs <- function() {

  df <- create_random_df()
  for (i in seq(1, 1000))
  {
    df <- rbind(df, create_random_df())
  }
  return (df)
}

#' Combines multiple random-row data frames using rbind.
#' It is to be compared to rbind_dfs, that does the
#' same without using Rcpp
#' @author Richel Bilderbeek
#' @export
rcpp_rbind_dfs <- function() {

  df <- create_random_df()
  for (i in seq(1, 1000))
  {
    df <- rbind(df, create_random_df())
  }
  return (df)
}
