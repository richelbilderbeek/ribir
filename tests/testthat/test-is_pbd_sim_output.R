context("is_pbd_sim_output")

test_that("basic tests", {

  if (rappdirs::app_dir()$os != "win") {
    sink(file.path(rappdirs::user_cache_dir(), "ddd"))
  } else {
    sink(rappdirs::user_cache_dir())
  }
  result <- is_pbd_sim_output(
    PBD::pbd_sim(c(0.2, 1, 0.2, 0.1, 0.1), 15),
    verbose = TRUE
  )
  sink()
  expect_true(result)


  if (rappdirs::app_dir()$os != "win") {
    sink(file.path(rappdirs::user_cache_dir(), "ddd"))
  } else {
    sink(rappdirs::user_cache_dir())
  }
  result <- is_pbd_sim_output(rep(x = 0, times = 9), verbose = TRUE)
  sink()

  expect_false(result)




  if (rappdirs::app_dir()$os != "win") {
    sink(file.path(rappdirs::user_cache_dir(), "ddd"))
  } else {
    sink(rappdirs::user_cache_dir())
  }
  result <- is_pbd_sim_output(
      as.list(rep(x = 0, times = 9)),
      verbose = TRUE
    )
  sink()
  expect_false(result)

  # Replace
  if (rappdirs::app_dir()$os != "win") {
    sink(file.path(rappdirs::user_cache_dir(), "ddd"))
  } else {
    sink(rappdirs::user_cache_dir())
  }
  result <- is_pbd_sim_output(
      as.list(rep(x = 0, times = 5)),
      verbose = TRUE
    )
  sink()
  expect_false(result)
})
