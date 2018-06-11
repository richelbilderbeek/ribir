context("add_outgroup_to_phylogeny")

test_that("use", {

  phylogeny <- add_outgroup_to_phylogeny(
    phylogeny = ape::rcoal(10),
    stem_length = 0.0
  )
  expect_equal(class(phylogeny), "phylo")
})

test_that("outgroup name", {

  outgroup_name <- "some name"

  phylogeny <- add_outgroup_to_phylogeny(
    phylogeny = ape::rcoal(10),
    stem_length = 0.0,
    outgroup_name = outgroup_name
  )

  expect_true(outgroup_name %in% phylogeny$tip.label)
})


test_that("abuse", {

  expect_error(
    add_outgroup_to_phylogeny(
      phylogeny = "I am not of class phylo", # Error
      stem_length = 0.0,
      outgroup_name = "Outgroup"
    ),
    "phylogeny must be a phylogeny"
  )
  expect_error(
    add_outgroup_to_phylogeny(
      phylogeny = ape::rcoal(10),
      stem_length = "I am not a length", # Error
      outgroup_name = "Outgroup"
    ),
    "stem_length must be a number"
  )

  expect_error(
    add_outgroup_to_phylogeny(
      phylogeny = ape::rcoal(10),
      stem_length = 0.0,
      outgroup_name = ape::rcoal(10) # Error
    ),
    "outgroup_name must be a word"
  )
})
