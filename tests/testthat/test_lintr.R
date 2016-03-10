test_that("Package must pass lintr", {
  skip("Only at a new release")
  if (requireNamespace("lintr", quietly = TRUE)) {
    context("lints")
    test_that("Package Style", {
      lintr::expect_lint_free()
    })
  }
})
