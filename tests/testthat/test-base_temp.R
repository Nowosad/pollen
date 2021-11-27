context("base_temp")

tavg <- c(25, 20, 15, 10)
d <- c(6, 11, 16, 21)
t1 <- base_temp(tavg = tavg, d = d, type = "sd_gdd")
t2 <- base_temp(tavg = tavg, d = d, type = "sd_day")
t3 <- base_temp(tavg = tavg, d = d, type = "cv_day")
t4 <- base_temp(tavg = tavg, d = d, type = "y_i")

test_that("base_temp works", {
  expect_is(c(t1, t2, t3, t4), "numeric")
  expect_true(all(c(t1, t2, t3, t4) > 0))
})
