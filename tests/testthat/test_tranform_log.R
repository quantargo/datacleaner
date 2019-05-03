context("tranform_log")
library(datacleaner)
test_that("tranform_log is correct", {
  expect_equal(as.character(transform_log(c(2, 2, 2, 2, 3))), as.character(c(0.693147180559945, 0.693147180559945, 0.693147180559945, 0.693147180559945, 1.09861228866811)))
  expect_equal(as.character(transform_log(c(2, 3, NA))), as.character(c(0.693147180559945, 1.09861228866811, NA)))
})

test_that("unexpected parameters", {
  expect_warning(t <- transform_log(c(2, 3, "NA")), "transform_log: Expecting numeric argument")
  expect_equal(as.character(t), as.character(c(0.693147180559945, 1.09861228866811, NA)))
})