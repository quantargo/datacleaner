context("NA and null values")
library(datacleaner)
test_that("NA and null values produce error message", {
  expect_error(windsorize(NA), "argument should not be a vector containing only NA-s or NULL-s")
  expect_error(windsorize(NULL), "argument should not be a vector containing only NA-s or NULL-s")
})