context("pseudoCluster-functions")

test_that("pseudoCluster-functions", {
  expect_equal(MALDIquantTools:::.pseudoCluster(c(1:3, 5, 8:10, 12, 15),
                                                chargeState=1, tolerance=100e-6,
                                                isotopicDistance=1),
               c(rep(TRUE, 3), FALSE, rep(TRUE, 3), rep(FALSE, 2)))
})

