context("is_phylogeny")

test_that("is_phylogeny: use", {
  expect_true(is_phylogeny(ape::rcoal(n = 5)))
  expect_false(is_phylogeny(ape::rmtree(N = 2, n = 10)))
  expect_false(is_phylogeny(42))
  expect_false(is_phylogeny(3.14))
  expect_false(is_phylogeny("Hello"))
})
