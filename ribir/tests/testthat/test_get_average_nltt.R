test_that("get_average_nltt: create some plots", {
  # The inner workings of get_average_nltt are done by get_average_nltt_matrix
  newick1 <- "((A:1,B:1):1,(C:1,D:1):1);"
  newick2 <- "((((XD:1,ZD:1):1,CE:2):1,(FE:2,EE:2):1):4,((AE:1,BE:1):1,(WD:1,YD:1):1):5);"
  phylogeny1 <- ape::read.tree(text = newick1)
  phylogeny2 <- ape::read.tree(text = newick2)
  ribir::get_average_nltt(c(phylogeny1,phylogeny2), dt = 0.20, plot_nltts = TRUE)
})
