test_that("vector is correctly windsorized", {
  expect_equal(windsorize(c(-1000,1,2,3,4,5,6,7,8,9,1000),0.9), c(-499.5,1,2,3,4,5,6,7,8,9,504.5) )
})
