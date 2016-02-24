test_that("get_average_nltt_matrix #1", {
  newick1 <- "((A:1,B:1):2,C:3);"
  newick2 <- "((A:2,B:2):1,C:3);"
  phylogeny1 <- ape::read.tree(text = newick1)
  phylogeny2 <- ape::read.tree(text = newick2)
  nltt_matrix1 <- get_phylogeny_nltt_matrix(phylogeny1)
  nltt_matrix2 <- get_phylogeny_nltt_matrix(phylogeny2)

  result <- ribir::get_average_nltt_matrix(c(phylogeny1,phylogeny2), dt = 0.20)


})
