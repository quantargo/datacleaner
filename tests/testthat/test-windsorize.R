test_that("windsorize works", {
  expect_equal(windsorize(3:10), c(3.35,4.00,5.00,6.00,7.00,8.00,9.00,9.65))
})
