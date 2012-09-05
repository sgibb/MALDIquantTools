context("binSpectrum-methods")

p <- createMassSpectrum(mass=1:10,
                        intensity=1:10)

r <- c(createMassSpectrum(mass=c(2.125, 4.375, 6.625, 8.875),
                          intensity=c(3, 5, 7, 10)),
       createMassSpectrum(mass=c(2.125, 4.375, 6.625, 8.875),
                          intensity=c(1, 4, 6, 8)))

test_that("binSpectrum,MassSpectrum", {
  expect_equal(r[[1]], MALDIquantTools:::.binSpectrum(p, nbins=4, from=1,
                                                      to=10))

  expect_equal(r[[2]], MALDIquantTools:::.binSpectrum(p, nbins=4, from=1, 
                                                      to=10, fun=min))
})

test_that("binSpectrum,list", {
  expect_equal(c(r[[1]], r[[1]]), 
               MALDIquantTools:::.binSpectrum(c(p, p), nbins=4))
})
