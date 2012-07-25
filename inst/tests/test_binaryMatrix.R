context("binaryMatrix-functions")

m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
b <- matrix(c(1, 1, 0, 1), nrow=2, ncol=2)

test_that("binaryMatrix", {
  expect_error(binaryMatrix(1:10))

  expect_equal(binaryMatrix(m), b)
})

test_that("isBinaryMatrix", {
  expect_true(!isBinaryMatrix(1:10))
  expect_true(!isBinaryMatrix(m))
  expect_true(isBinaryMatrix(b))
})
