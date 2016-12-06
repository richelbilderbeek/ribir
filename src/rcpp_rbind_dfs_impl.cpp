#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
rcpp_rbind_dfs_impl <- function() {

  df <- create_random_df()
  for (i in seq(1, 1000))
  {
    df <- rbind(df, create_random_df())
  }
  return (df)

}
