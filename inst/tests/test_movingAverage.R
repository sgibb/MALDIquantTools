context("movingAverage-functions")

test_that("movingAverage", {
  expect_error(movingAverage(1:10))
  expect_error(movingAverage(1:10, 100))

  values <- 1:10
  values[c(1, 2, 9, 10)] <- NA
  expect_equal(movingAverage(1:10, 2), values)

  values <- 1:10
  values[c(1, 2, 3, 8, 9, 10)] <- NA
  expect_equal(movingAverage(1:10, 3), values)
})
