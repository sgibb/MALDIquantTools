context("mqReadMzAscii-functions")

test_that("mqReadMzAscii", {
  expect_error(mqReadMzAscii("tmp.tmp"))

  single <- mqReadMzAscii("data/ascii1.txt")

  expect_equal(mass(single[[1]]), 1:5)
  expect_equal(intensity(single[[1]]), 6:10)
  expect_equal(basename(metaData(single[[1]])$file), "ascii1.txt")

  multi <- mqReadMzAscii("data/")

  for (i in seq(along=multi)) {
    expect_equal(mass(multi[[i]]), 1:5)
    expect_equal(intensity(multi[[i]]), 6:10)
    expect_equal(basename(metaData(multi[[i]])$file), 
                 paste("ascii", i, ".txt", sep=""))
  }
})

test_that("mqReadMzCsv", {
  expect_error(mqReadMzCsv("tmp.tmp"))

  single <- mqReadMzCsv("data/csv1.csv")
 
  expect_equal(mass(single[[1]]), 1:5)
  expect_equal(intensity(single[[1]]), 6:10)
  expect_equal(basename(metaData(single[[1]])$file), "csv1.csv")

  multi <- mqReadMzCsv("data/")

  for (i in seq(along=multi)) {
    expect_equal(mass(multi[[i]]), 1:5)
    expect_equal(intensity(multi[[i]]), 6:10)
    expect_equal(basename(metaData(multi[[i]])$file), 
                 paste("csv", i, ".csv", sep=""))
  }
})
