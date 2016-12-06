context("rcpp_rbind_dfs")

test_that("rbind_dfs and rcpp_rbind_dfs have same output", {

  set.seed(42)
  df1 <- rbind_dfs()
  set.seed(42)
  df2 <- rcpp_rbind_dfs()
  expect_equal(df1, df2)
})

test_that("rbind_dfs and rcpp_rbind_dfs have diffent times", {

  set.seed(42)
  t1 <- system.time(replicate(1, rbind_dfs()))
  set.seed(42)
  t2 <- system.time(replicate(1, rcpp_rbind_dfs()))
  #Except a ten-fold difference at least
  expect_true(t1[1] > 10 * t2[1])
})



