source("../../lib/activity.R")

test_that("is.weekday works", {
  days <- c("2015-05-26", "2015-05-24")
  expect_equal(is.weekday(days), c(TRUE, FALSE))
  expect_equal(is.weekday(c("")), c(FALSE))
})