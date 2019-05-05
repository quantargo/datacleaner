test_that("Vector is correctly log-transformed", {
  expect_equal(transform_log(c(1,1,1)), c(0,0,0))
})
