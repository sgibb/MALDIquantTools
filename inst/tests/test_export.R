context("export")

s <- c(createMassSpectrum(mass=1:5, intensity=6:10, list(fullName="sample1")),
       createMassSpectrum(mass=1:5, intensity=6:10, list(fullName="sample2")))

test_that("write.table", {
  tmp <- tempfile()
  write.table(x=s[[1]], file=tmp)
  expect_equal(read.table(tmp), read.table("data/ascii1.txt"))
})

test_that("export", {
  tmp <- tempfile()
  export(x=s[[1]], file=tmp)
  expect_equal(read.table(tmp), read.table("data/ascii1.txt"))
})

test_that("write.csv", {
  tmp <- tempfile()
  write.csv(x=s[[1]], file=tmp)
  expect_equal(read.csv(tmp), read.csv("data/csv1.csv"))
})

test_that("exportCsv", {
  tmp <- tempfile()
  exportCsv(x=s[[1]], file=tmp)
  expect_equal(read.csv(tmp), read.csv("data/csv1.csv"))
})

test_that("export", {
  tmp <- tempdir()
  export(x=s, path=tmp)
  expect_equal(read.table(file.path(tmp, "sample1.txt")), read.table("data/ascii1.txt"))
  expect_equal(read.table(file.path(tmp, "sample2.txt")), read.table("data/ascii1.txt"))
})

test_that("write.csv", {
  tmp <- tempdir()
  write.csv(x=s, path=tmp)
  expect_equal(read.csv(file.path(tmp, "sample1.csv")), read.csv("data/csv1.csv"))
  expect_equal(read.csv(file.path(tmp, "sample2.csv")), read.csv("data/csv1.csv"))
})

test_that("exportCsv", {
  tmp <- tempdir()
  exportCsv(x=s, path=tmp)
  expect_equal(read.csv(file.path(tmp, "sample1.csv")), read.csv("data/csv1.csv"))
  expect_equal(read.csv(file.path(tmp, "sample2.csv")), read.csv("data/csv1.csv"))
})
