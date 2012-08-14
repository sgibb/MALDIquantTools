context("individualReferencePeaks-functions")

p <- list(createMassPeaks(mass=1:5, intensity=1:5),
          createMassPeaks(mass=c(1, 2:4+0.2, 5.001), intensity=1:5),
          createMassPeaks(mass=c(1, 2:4+0.4, 5.02), intensity=1:5))

r1 <- list(createMassPeaks(mass=c(1, 5), intensity=c(1, 5)),
           createMassPeaks(mass=c(1, 5.001), intensity=c(1, 5)),
           createMassPeaks(mass=c(1, 5.02), intensity=c(1, 5)))

r2 <- list(createMassPeaks(mass=c(1, 5), intensity=c(1, 5)),
           createMassPeaks(mass=c(1, 5.001), intensity=c(1, 5)),
           createMassPeaks(mass=1, intensity=1))

test_that("individualReferencePeaks", {
  expect_error(individualReferencePeaks(list()))
  expect_equal(individualReferencePeaks(p, tolerance=0.02), r1)
  expect_equal(individualReferencePeaks(p, tolerance=0.002), r2)
})
