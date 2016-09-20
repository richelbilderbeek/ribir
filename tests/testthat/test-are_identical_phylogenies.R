context("are_identical_phylogenies")

test_that("are_identical_phylogenies: use", {
  p <- ape::rcoal(10)
  q <- ape::rcoal(10)
  expect_true(are_identical_phylogenies(p, p))
  expect_false(are_identical_phylogenies(p, q))
  expect_false(are_identical_phylogenies(q, p))
  expect_true(are_identical_phylogenies(q, q))
})

test_that("are_identical_phylogenies: abuse", {
  p <- ape::rcoal(10)
  expect_error(
    are_identical_phylogenies("not a phylogeny", p),
    "p must be a phylogeny"
  )
  expect_error(
    are_identical_phylogenies(p, "not a phylogeny"),
    "q must be a phylogeny"
  )
})
