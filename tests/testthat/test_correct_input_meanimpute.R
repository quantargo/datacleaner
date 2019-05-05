test_that("Incorrect input of meanimpute", {
  #tests related to input vector
  expect_error(meanimpute(NULL),"Input vector cannot be NULL.")
  expect_error(meanimpute(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)),"Input vector should contain at least one numeric element.")
  expect_error(meanimpute(c(1,2,3,4,"Dracula"), "Input vector should contain at least one numeric element."))

})
