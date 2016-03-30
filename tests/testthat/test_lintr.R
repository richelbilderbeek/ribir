test_that("Package must pass lintr", {
  skip("Only at a new release")
  lintr::expect_lint_free()
})
