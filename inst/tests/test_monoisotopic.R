context("monoisotopic-methods")

p <- createMassPeaks(mass=c(1:5, 9, 20:24, 30:34),
                     intensity=c(5:1, 1, 3, 5:2, 0.01, 2, 3:1))

r <- createMassPeaks(mass=c(1, 20, 31),
                     intensity=c(5, 3, 2))

referenceTable <- data.frame(monoisotopicMass=c(1.01, 20.2, 29.1),
                             relativeIntensityApexToMonoisotopic=c(1, 5/3, 3/2),
                             apexIdx=c(1, 2, 2))

test_that("monoisotopic,MassPeaks", {
  expect_equal(monoisotopic(p, isotopicDistance=1, chargeState=1,
                            referenceTable=referenceTable),
               r)
})

test_that("monoisotopic,list", {
  expect_equal(monoisotopic(list(p, p), chargeState=1,
                            referenceTable=referenceTable),
               list(r, r))
})
