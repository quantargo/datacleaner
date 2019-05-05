test_that("NA's in vector are correctly replace by mean", {
  expect_equal(meanimpute(c(2,4,6,NA)), c(2,4,6,4))
})
