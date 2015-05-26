source("../../lib/activity.R")

test_that("is.weekday works", {
  expect_equal(2-1, 1)
  days <- c("2015-05-26")

  expect_equal(is.weekday(days), c(TRUE))
})