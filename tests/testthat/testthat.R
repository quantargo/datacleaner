library(datacleaner)

test_that("windsorize the input vector", {
  expect_error(windsorize(c(NULL)), "Input x is NULL")
  expect_error(windsorize(c(1, 2, NA)), "Input x contains value NA")
  expect_equal(windsorize(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 0.8), c(1.9, 2, 3, 4, 5, 6, 7, 8, 9, 9.1))
})

test_that("Takes natural logarithm", {
  expect_error(transform_log(c(NULL)), "Input x is NULL")
  expect_error(transform_log(c(1, 2, NA)), "Input x contains value NA")
  expect_error(transform_log(c(-1, 2, 3)), "Input x contains non-positive values")
  expect_equal(transform_log(c(1, exp(1))), c(0, 1))
})

test_that("Takes natural logarithm", {
  expect_equal(meanimpute(c(1, NA, 3)), c(1, 2, 3))
  expect_equal(meanimpute(c(1, 2 , 3)), c(1, 2, 3))
})