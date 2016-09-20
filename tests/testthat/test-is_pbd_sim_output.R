context("is_pbd_sim_output")

test_that("basic tests", {
  sink("/dev/null") # nolint
  result <- is_pbd_sim_output(
    PBD::pbd_sim(c(0.2, 1, 0.2, 0.1, 0.1), 15),
    verbose = TRUE
  )
  sink()
  expect_true(result)

  sink("/dev/null") # nolint
  result <- is_pbd_sim_output(rep(x = 0, times = 9), verbose = TRUE)
  sink()

  expect_false(result)

  sink("/dev/null") # nolint
  result <- is_pbd_sim_output(
      as.list(rep(x = 0, times = 9)),
      verbose = TRUE
    )
  sink()
  expect_false(result)

  # Replace
  sink("/dev/null") # nolint
  result <- is_pbd_sim_output(
      as.list(rep(x = 0, times = 5)),
      verbose = TRUE
    )
  sink()
  expect_false(result)
})
