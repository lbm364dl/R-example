library("testthat")

# Worth reading https://r-pkgs.org/testing-basics.html
# Used library https://testthat.r-lib.org/index.html

# TODO: example test
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
