context("rkt_calc_harm_mean")

test_that("use", {
  expect_equal(rkt_calc_harm_mean(c(1.2)), 1.2)
  expect_equal(rkt_calc_harm_mean(c(1.0, 2.0)), 1.33333333)
})
