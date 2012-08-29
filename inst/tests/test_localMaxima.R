context("localMaxima-functions")

test_that("localMaxima", {
  expect_equal(MALDIquantTools:::.localMaxima(c(1, 2, 1, 2, 1)),
               c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(MALDIquantTools:::.localMaxima(rep(1, 5)),
               rep(FALSE, 5))
})
