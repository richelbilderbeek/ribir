test_that(paste("get_nltt_values: ",
  "How to stretch an nLTT timepoints matrix: ",
  "Example: Easy tree", sep = ""), {

  # The average of nLTTs A and B should be C
  #
  #      A              B              C       # nolint
  #                                            # nolint
  # |  ********    |      ****    |      ****  # nolint
  # |  *           |      *       |   ****     # nolint
  # ****           ********       *****        # nolint
  # |              |              |            # nolint
  # |              |              |            # nolint
  # |              |              |            # nolint
  # +----------    +----------    +----------  # nolint
  #
  newick1 <- "((A:1,B:1):2,C:3);"
  newick2 <- "((A:2,B:2):1,C:3);"
  phylogeny1 <- ape::read.tree(text = newick1)
  phylogeny2 <- ape::read.tree(text = newick2)
  n_phylogenies <- 2
  nltt_matrix1 <- ribir::stretch_nltt_matrix(
    get_phylogeny_nltt_matrix(phylogeny1),
    dt = 0.2, step_type = "upper")
  ##      [,1]      [,2]  # nolint
  ## [1,]  0.0 0.6666667  # nolint
  ## [2,]  0.2 0.6666667  # nolint
  ## [3,]  0.4 0.6666667  # nolint
  ## [4,]  0.6 0.6666667  # nolint
  ## [5,]  0.8 1.0000000  # nolint
  ## [6,]  1.0 1.0000000  # nolint
  expected_nltt_matrix1 <- matrix(c(seq(0.0, 1.0, 0.2),
    rep(2 / 3, 4), rep(1, 2)), ncol = 2)
  testit::assert(all.equal(nltt_matrix1, expected_nltt_matrix1))

  nltt_matrix2 <- ribir::stretch_nltt_matrix(
    get_phylogeny_nltt_matrix(phylogeny2),
    dt = 0.2, step_type = "upper")
  ##      [,1]      [,2]  # nolint
  ## [1,]  0.0 0.6666667  # nolint
  ## [2,]  0.2 0.6666667  # nolint
  ## [3,]  0.4 1.0000000  # nolint
  ## [4,]  0.6 1.0000000  # nolint
  ## [5,]  0.8 1.0000000  # nolint
  ## [6,]  1.0 1.0000000  # nolint
  expected_nltt_matrix2 <- matrix(c(seq(0.0, 1.0, 0.2),
    rep(2 / 3, 2), rep(1, 4)), ncol = 2)
  testit::assert(all.equal(nltt_matrix2, expected_nltt_matrix2))

  phylogenies <- c(phylogeny1, phylogeny2)
  testit::assert(length(phylogenies) == 2)
  result <- ribir::get_nltt_values_unmelted(
    phylogenies, dt = 0.20)
  ##      [,1]      [,2]       [,3]  # nolint
  ## [1,]  0.0 0.6666667  0.6666667  # nolint
  ## [2,]  0.2 0.6666667  0.6666667  # nolint
  ## [3,]  0.4 0.6666667  1.0000000  # nolint
  ## [4,]  0.6 0.6666667  1.0000000  # nolint
  ## [5,]  0.8 1.0000000  1.0000000  # nolint
  ## [6,]  1.0 1.0000000  1.0000000  # nolint
  m <- matrix(c(seq(0.0, 1.0, 0.2),
    rep(2 / 3, 4), rep(1, 2), rep(2 / 3, 2), rep(1, 4)), ncol = 3)
  expected <- as.data.frame(x = m)
  colnames(expected) <- c("time", paste(rep("N", 2),
    as.character(seq(1, n_phylogenies)), sep = ""))
  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 3)
  expect_equal(all.equal(result, expected), TRUE)
})





test_that(paste("get_nltt_values: ",
  "How to stretch an nLTT timepoints matrix: ",
  "Example: Harder trees", sep = ""), {

  newick1 <- "((A:1,B:1):1,(C:1,D:1):1);"
  newick2 <- paste("((((XD:1,ZD:1):1,CE:2):1,(FE:2,EE:2):1):4,",
    "((AE:1,BE:1):1,(WD:1,YD:1):1):5);", sep = "")
  phylogeny1 <- ape::read.tree(text = newick1)
  phylogeny2 <- ape::read.tree(text = newick2)
  n_phylogenies <- 2

  nltt_matrix1 <- ribir::stretch_nltt_matrix(
    ribir::get_phylogeny_nltt_matrix(phylogeny1),
    dt = 0.20, step_type = "upper")

  ##      [,1] [,2]  # nolint
  ## [1,]  0.0  0.5  # nolint
  ## [2,]  0.2  0.5  # nolint
  ## [3,]  0.4  0.5  # nolint
  ## [4,]  0.6  1.0  # nolint
  ## [5,]  0.8  1.0  # nolint
  ## [6,]  1.0  1.0  # nolint
  expected_nltt_matrix1 <- matrix(c(seq(0.0, 1.0, 0.2),
    rep(0.5, 3), rep(1.0, 3)), ncol = 2)
  testit::assert(all.equal(nltt_matrix1, expected_nltt_matrix1))

  nltt_matrix2 <- ribir::stretch_nltt_matrix(
    ribir::get_phylogeny_nltt_matrix(phylogeny2),
    dt = 0.20, step_type = "upper")
  ##      [,1]      [,2]  # nolint
  ## [1,]  0.0 0.2222222  # nolint
  ## [2,]  0.2 0.2222222  # nolint
  ## [3,]  0.4 0.2222222  # nolint
  ## [4,]  0.6 0.3333333  # nolint
  ## [5,]  0.8 0.6666667  # nolint
  ## [6,]  1.0 1.0000000  # nolint
  expected_nltt_matrix2 <- matrix(c(seq(0.0, 1.0, 0.2),
    rep(2 / 9, 3), 1 / 3, 2 / 3, 1.0), ncol = 2)
  testit::assert(all.equal(nltt_matrix2, expected_nltt_matrix2))
  #phylogenies <- c(phylogeny1, phylogeny2)  # nolint

  # The real tests
  result <- ribir::get_nltt_values_unmelted(
    c(phylogeny1, phylogeny2), dt = 0.20)

  ##      [,1] [,2]  # nolint
  ## [1,]  0.0  0.5  0.2222222 # nolint
  ## [2,]  0.2  0.5  0.2222222 # nolint
  ## [3,]  0.4  0.5  0.2222222 # nolint
  ## [4,]  0.6  1.0  0.3333333 # nolint
  ## [5,]  0.8  1.0  0.6666667 # nolint
  ## [6,]  1.0  1.0  1.0000000 # nolint
  m <- matrix(c(seq(0.0, 1.0, 0.2),
    rep(0.5, 3), rep(1.0, 3), rep(2 / 9, 3), 1 / 3, 2 / 3, 1.0), ncol = 3)
  expected <- as.data.frame(x = m)
  colnames(expected) <- c("time", paste(rep("N", 2),
    as.character(seq(1, n_phylogenies)), sep = ""))

  expect_equal(all.equal(result, expected), TRUE)
})
