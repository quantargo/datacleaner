context("No negative values")
library(datacleaner)
test_that("There are no negative values in input", {
  expect_error(transform_log(c(1,2,-1)), "input can't be negative")
})