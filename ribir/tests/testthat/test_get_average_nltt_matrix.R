test_that("get_average_nltt_matrix: How to stretch an nLTT timepoints matrix: Example: Easy tree", {

  # The average of nLTTs A and B should be C
  #
  #      A              B              C
  #
  # |  ********    |      ****    |      ****
  # |  *           |      *       |   ****
  # ****           ********       *****
  # |              |              |
  # |              |              |
  # |              |              |
  # +----------    +----------    +----------
  #
  newick1 <- "((A:1,B:1):2,C:3);"
  newick2 <- "((A:2,B:2):1,C:3);"
  phylogeny1 <- ape::read.tree(text = newick1)
  phylogeny2 <- ape::read.tree(text = newick2)
  nltt_matrix1 <- ribir::stretch_nltt_matrix(
    get_phylogeny_nltt_matrix(phylogeny1),
    dt = 0.2, step_type = "upper")
  ##      [,1]      [,2]
  ## [1,]  0.0 0.6666667
  ## [2,]  0.2 0.6666667
  ## [3,]  0.4 0.6666667
  ## [4,]  0.6 0.6666667
  ## [5,]  0.8 1.0000000
  ## [6,]  1.0 1.0000000
  expected_nltt_matrix1 <- matrix(c(seq(0.0,1.0,0.2),rep(2/3,4),rep(1,2)), ncol = 2)
  testit::assert(all.equal(nltt_matrix1,expected_nltt_matrix1))

  nltt_matrix2 <- ribir::stretch_nltt_matrix(
    get_phylogeny_nltt_matrix(phylogeny2),
    dt = 0.2, step_type = "upper")
  ##      [,1]      [,2]
  ## [1,]  0.0 0.6666667
  ## [2,]  0.2 0.6666667
  ## [3,]  0.4 1.0000000
  ## [4,]  0.6 1.0000000
  ## [5,]  0.8 1.0000000
  ## [6,]  1.0 1.0000000
  expected_nltt_matrix2 <- matrix(c(seq(0.0,1.0,0.2),rep(2/3,2),rep(1,4)), ncol = 2)
  testit::assert(all.equal(nltt_matrix2,expected_nltt_matrix2))

  result <- ribir::get_average_nltt_matrix(c(phylogeny1,phylogeny2), dt = 0.20)
  ##      [,1]      [,2]
  ## [1,]  0.0 0.6666667
  ## [2,]  0.2 0.6666667
  ## [3,]  0.4 0.8333333
  ## [4,]  0.6 0.8333333
  ## [5,]  0.8 1.0000000
  ## [6,]  1.0 1.0000000
  expected <- matrix(c(seq(0.0,1.0,0.2),rep(2/3,2),rep(5/6,2),rep(1,2)), ncol = 2)
  expect_equal(all.equal(nltt_matrix2,expected_nltt_matrix2),TRUE)
})
