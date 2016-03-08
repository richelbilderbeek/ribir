test_that("Package must pass lintr", {

  lints <- devtools::lint()
  print(lints)
  expect_equal(length(lints), 0)
})
