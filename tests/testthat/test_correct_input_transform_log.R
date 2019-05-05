test_that("Input of transform_log() is correct.", {
  #tests related to input vector
  expect_error(transform_log(NULL), "Input vector is not allowed to be NULL.")
  expect_error(transform_log(c(NA,NA,NA,6,NA,5,NA,NA,7,NA,NA)), "There is at least one NA value in input vector.")
  expect_error(transform_log(c(1,2,3,4,5,6,7,8,"string",1000)), "There is at least one non-numeric value.")
  expect_error(transform_log(c(1,2,3,4,5,6,7,8,-5)), "There is at least one negative value.")
  
})
