context("filename-functions")

test_that("cleanFilename", {
  expect_equal("/home/a:/\"foo&bar\"/g.\\23!/ foo-bar?.txt",
               "_home_a_foo_bar_g_23_foo_bar_txt")
})
