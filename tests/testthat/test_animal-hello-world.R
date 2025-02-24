library("testthat")

test_that("my_hello_world generated ASCII art contains Hello world!", {
  output <- my_hello_world("cow")
  say_msg <- "Hello world!"
  expect_equal(grepl(say_msg, output), TRUE)
  expect_true(grepl(say_msg, output))
  expect_false(grepl(paste0(say_msg, "!"), output))

  len_output <- stringr::str_count(output)
  len_say_msg <- stringr::str_count(say_msg)
  expect_false(len_output == len_say_msg)
})
