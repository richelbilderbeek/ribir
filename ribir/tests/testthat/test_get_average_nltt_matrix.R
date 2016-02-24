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





test_that("get_average_nltt_matrix: How to stretch an nLTT timepoints matrix: Example: Harder trees", {

  newick1 <- "((A:1,B:1):1,(C:1,D:1):1);"
  newick2 <- "((((XD:1,ZD:1):1,CE:2):1,(FE:2,EE:2):1):4,((AE:1,BE:1):1,(WD:1,YD:1):1):5);"
  phylogeny1 <- ape::read.tree(text = newick1)
  phylogeny2 <- ape::read.tree(text = newick2)

  nltt_matrix1 <- ribir::stretch_nltt_matrix(ribir::get_phylogeny_nltt_matrix(phylogeny1), dt = 0.20, step_type = "upper")

  ##      [,1] [,2]
  ## [1,]  0.0  0.5
  ## [2,]  0.2  0.5
  ## [3,]  0.4  0.5
  ## [4,]  0.6  1.0
  ## [5,]  0.8  1.0
  ## [6,]  1.0  1.0
  expected_nltt_matrix1 <- matrix(c(seq(0.0,1.0,0.2),rep(0.5,3),rep(1.0,3)), ncol = 2)
  testit::assert(all.equal(nltt_matrix1,expected_nltt_matrix1))

  nltt_matrix2 <- ribir::stretch_nltt_matrix(ribir::get_phylogeny_nltt_matrix(phylogeny2), dt = 0.20, step_type = "upper")
  ##      [,1]      [,2]
  ## [1,]  0.0 0.2222222
  ## [2,]  0.2 0.2222222
  ## [3,]  0.4 0.2222222
  ## [4,]  0.6 0.3333333
  ## [5,]  0.8 0.6666667
  ## [6,]  1.0 1.0000000
  expected_nltt_matrix2 <- matrix(c(seq(0.0,1.0,0.2),rep(2/9,3),1/3,2/3,1.0), ncol = 2)
  testit::assert(all.equal(nltt_matrix2,expected_nltt_matrix2))

  result <- ribir::get_average_nltt_matrix(c(phylogeny1,phylogeny2), dt = 0.20)

  ##      [,1]      [,2]
  ## [1,]  0.0 0.3611111
  ## [2,]  0.2 0.3611111
  ## [3,]  0.4 0.3611111
  ## [4,]  0.6 0.6666667
  ## [5,]  0.8 0.8333333
  ## [6,]  1.0 1.0000000
  expected <- matrix(c(seq(0.0,1.0,0.2),rep(13/36,3),2/3,5/6,1.0), ncol = 2)
  expect_equal(all.equal(nltt_matrix2,expected_nltt_matrix2),TRUE)
})


test_that("get_average_nltt_matrix: data types", {
  # Create a list or multiPhylo of phylogenies (of type phylo)
  # and run it through the get_average_nltt_matrix function

  n_trees <- 2
  n_tips <- 3
  set.seed(41)
  ape_phylogenies <- ape::rmtree(N = n_trees, n = n_tips)
  m <- get_average_nltt_matrix(ape_phylogenies)
  #plot(m)
  expect_equal(ncol(m),2)
  expect_equal(nrow(m),1001)

  set.seed(41)
  treesim_phylogenies <- TreeSim::sim.bd.age(6, numbsim = n_trees, lambda = 0.4, mu = 0.0, complete = FALSE)
  n <- get_average_nltt_matrix(treesim_phylogenies)
  #plot(n)
  expect_equal(ncol(n),2)
  expect_equal(nrow(n),1001)

  combined_phylogenies <- c(ape::rcoal(10),ape::rcoal(20))
  p <- get_average_nltt_matrix(combined_phylogenies)
  #plot(p)
  expect_equal(ncol(p),2)
  expect_equal(nrow(p),1001)
  expect_equal(TRUE,TRUE)
})

test_that("get_average_nltt_matrix: speed comparison", {
  skip_on_cran()
  if (FALSE) {
    ??ape::multiPhylo
    ?li
    phylogenies <- ape::rmtree(N = n_trees, n = n_tips)
    ?ape::rmtree
    phylogenies
    plot(phylogenies[1])
    ??ape::drop.fossil
    class(phylogenies)
    class(phylogenies[[1]])

    a <- replicate(N, rtree(n, rooted = rooted, tip.label = tip.label,
      br = br, ...), simplify = FALSE)

    phylogenies <- TreeSim::sim.bd.age(10, numbsim = n_trees, lambda = 0.5, mu = 0.1, complete = FALSE)
    phylogenies
    ?TreeSim::sim.bd.age
    length(phylogenies)


    phylogenies <- lapply(phylogenies, FUN = function(x) { class(x) <- "phylo"; return (x) } )
    class(phylogenies) <- "multiPhylo"
    testit::assert(class(phylogenies) == "multiPhylo")
    testit::assert(class(phylogenies[[1]]) == "phylo")
    plot(phylogenies[1])
    testit::assert(is.ultrametric(phylogenies[[1]]))
    get_average_nltt(phylogenies)

    plot(phylogenies[2])
    ?rep
  }
  expect_equal(TRUE, TRUE)
})
