context("fasta_to_phylo")

test_that("example", {

  fasta_filename <- beautier::get_fasta_filename()
  crown_age <- 25.0
  phylos <- fasta_to_phylo(fasta_filename, crown_age = crown_age)
  expect_equal(5, length(phylos$tip.label))
  expect_equal(crown_age,
    beautier::get_crown_age(phylos)
  )
})

test_that("abuse", {

  expect_error(
    fasta_to_phylo(fasta_filename = "absent", crown_age = 15)
  )

  expect_error(
    fasta_to_phylo(
      fasta_filename = beautier::get_fasta_filename(),
      crown_age = -42)
  )
})
