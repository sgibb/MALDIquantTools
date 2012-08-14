context("binningCosts-functions")

pre <- list(a=createMassPeaks(mass=1:5, intensity=1:5),
            b=createMassPeaks(mass=1:5+0.1, intensity=1:5),
            c=createMassPeaks(mass=1:5+0.2, intensity=1:5))
post <- binPeaks(pre, tolerance=0.2)

relative <- list(a=c(0.090909090909091, 0.0476190476190477, 0.0322580645161291,
                     0.0243902439024389, 0.0196078431372548),
                 b=c(0, 0, 0, 0, 0),
                 c=c(0.0909090909090908, 0.0476190476190477, 0.0322580645161291,
                     0.0243902439024392, 0.019607843137255))

absolute <- list(a=rep(0.1, 5), b=rep(0, 5), c=rep(0.1, 5))

test_that("binningCosts", {
  expect_error(binningCosts(pre, list()))
  expect_error(binningCosts(list(), post))
  expect_error(binningCosts(pre, post[-1]))

  expect_equal(binningCosts(pre, post), relative)
  expect_equal(binningCosts(pre, post, relative=TRUE), relative)
  expect_equal(binningCosts(pre, post, relative=FALSE), absolute)
})
