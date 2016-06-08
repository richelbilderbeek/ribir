context("add_outgroup_to_phylogeny")

test_that("phylogeny must be of class phylo", {
  expect_error(
    add_outgroup_to_phylogeny(
      phylogeny = "I am not of class phylo", # Ouch
      stem_length = 0.0, # OK
      outgroup_name = "Outgroup" # OK
    )
  )
})

test_that("stem_length must be a number", {
  expect_error(
    add_outgroup_to_phylogeny(
      phylogeny = rcoal(10), #OK
      stem_length = "I am not a length", # Ouch
      outgroup_name = "Outgroup" # OK
    )
  )
})

test_that("outgroup_name must be a character", {
  expect_error(
    add_outgroup_to_phylogeny(
      phylogeny = rcoal(10), #OK
      stem_length = 0.0, #OK
      outgroup_name = rcoal(10) # Ouch
    )
  )
})

test_that("result is of class phylo", {

  phylogeny <- add_outgroup_to_phylogeny(
    phylogeny = ape::rcoal(10), #OK
    stem_length = 0.0, # OK
    outgroup_name = "Outgroup" # OK
  )
  expect_equal(class(phylogeny), "phylo")
})
