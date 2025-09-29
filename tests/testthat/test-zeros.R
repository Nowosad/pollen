context("zeros")

test_that("zero is zero", {
  expect_true(is_zero(0))
  expect_false(is_zero(0.0000000001))
  expect_false(is_zero(111))
  expect_false(is_zero(-111))
  expect_false(is_zero(TRUE))
  expect_true(all(is_zero(c(0, 0))))
  expect_false(is_zero("111"))
})
