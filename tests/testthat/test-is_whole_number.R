context("is_whole_number")

test_that("basic usage", {
  expect_true(is_whole_number(42))
  expect_false(is_whole_number(42.01))
  expect_false(is_whole_number("Hello"))
  expect_false(is_whole_number(c(1, 2)))
})
