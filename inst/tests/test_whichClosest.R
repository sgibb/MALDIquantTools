context("whichClosest-functions")

test_that("whichClosest", {
  expect_true(MALDIquantTools:::.whichClosest(db=(1:10)+0.2, key=2.1) == 2)
  expect_true(MALDIquantTools:::.whichClosest(db=(1:10), key=2) == 2)
})
