library(nLTT)
library(testit)
# library(TreeSim)
#library(devtools)
#devtools::load_all()
#devtools::use_testthat()


test_that("stretch_nltt_matrix simple", {
  # t   N      t   N
  # 0.0 0.5    0.0 0.5
  #         -> 0.5 0.5
  # 1.0 1.0    1.0 1.0
  m <- matrix( c(c(0.0,1.0), c(0.5,1.0)), ncol = 2, nrow = 2)
  colnames(m) <- c("t","N")
  expected <- matrix(
    c(
      seq(0.0,1.0,0.5),
      c(0.5,0.5,1.0)
    ),
    ncol = 2, nrow = 3
  )
  result <- stretch_nltt_matrix(m = m, dt = 1.0/2)
  if (FALSE) {
    print("RESULT")
    print(result)
    print("EXPECTED")
    print(expected)
  }
  expect_equal(identical(result,expected), TRUE)
})

test_that("stretch_nltt_matrix 2", {
  # Fill in the timepoints:
  #
  # t   N
  # 0.0 0.2
  # 0.4 0.5
  # 1.0 1.0
  #
  # becomes
  #
  # t   N
  # 0.0 0.2
  # 0.1 0.2
  # 0.2 0.2
  # 0.3 0.2
  # 0.4 0.5
  # 0.5 0.5
  # 0.6 0.5
  # 0.7 0.5
  # 0.8 0.5
  # 0.9 0.5
  # 1.0 1.0

  test <- matrix( c(c(0.0,0.4,1.0), c(0.2,0.5,1.0)), ncol = 2, nrow = 3)
  colnames(test) <- c("t","N")
  expected <- matrix(
    c(
      seq(0.0,1.0,0.1),
      rep(0.2,times = 4),rep(0.5, times = 6), 1.0),
      ncol = 2, nrow = 11
  )
  result <- stretch_nltt_matrix(m = test, dt = 1.0/10)
  if (FALSE) {
    print("RESULT")
    print(result)
    print("EXPECTED")
    print(expected)
  }
  expect_equal(identical(result,expected), TRUE)
})
