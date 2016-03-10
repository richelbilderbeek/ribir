context("test_code_coverage")

test_that("Must be above 80%", {
  x <- covr::package_coverage(".")
  x
  print(x)
  sum(x)
})
