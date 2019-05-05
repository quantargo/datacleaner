test_that("Incorrect input", {
  #tests related to input vector
  expect_error(windsorize(NULL, .9), "Input vector cannot be NULL.")
  expect_error(windsorize(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), .9), "There should be no NA's in input vector.")
  expect_error(windsorize(c(-1000,1,2,3,4,5,6,7,8,"string",1000), .9), "There should only numeric values in the input vector.")
  expect_error(windsorize(c(-1000,1,2,3,4,6,7,8,9,1000), "string"), "Input quantile should be a number between 0 and 1.")
  expect_error(windsorize(c(-1000,1,2,3,4,6,7,8,9,1000), 2), "Input quantile should be a number between 0 and 1")
  expect_error(windsorize(c(-1000,1,2,3,4,6,7,8,9,1000), -2), "Input quantile should be a number between 0 and 1")
  
})
