context("is_distributed_normally")

test_that("is_distributed_normally: use", {
  # Create a normal disribution
  nd <- rnorm(n = 1000, mean = 0.0, sd = 1.0)
  expect_true(is_distributed_normally(nd))

  # Create a non-normal disribution
  nnd <- runif(n = 1000, min = 0.0, max = 1.0)
  expect_false(is_distributed_normally(nnd))

})

