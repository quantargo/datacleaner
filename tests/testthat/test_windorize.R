context("Windsorize")
library(datacleaner)
test_that("windorizing is correct", {
  expect_equal(windsorize(c(2,2,2,2,3),.9), c(2,2,2,2,2.8) )
  expect_equal(windsorize(c(2,2,2,2,1),.9), c(2,2,2,2,1.2) )
})

test_that("unexpected parameters", {
  expect_error(windsorize(c(NA,NA,NA), .9), "argument should not be a vector containing only NA")
  expect_error(windsorize(c(), .9), "argument should not be a empty vector")
  expect_error(windsorize(c(1,2,3,"4"), .9), "argument should be a numeric vector")
  expect_error(windsorize(c(1,2,3,4), ".9"), "argument should be a number from 0 to 1")
  expect_error(windsorize(c(1,2,3,4), -1), "p invalid percentale. Expected values from 0 to 1")
  expect_error(windsorize(c(1,2,3,4), 1.2), "p invalid percentale. Expected values from 0 to 1")
  expect_error(windsorize(c(1,2,3,NA), .9))
  
})
