library("testthat")

# Worth reading https://r-pkgs.org/testing-basics.html
# Used library https://testthat.r-lib.org/index.html

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("count_starwars_sexes gives correct count", {
  expected_occurrences <- tibble::tibble(
    sex = c("female", "hermaphroditic", "male", "none", NA),
    n = c(16, 1, 60, 6, 4)
  )
  expect_equal(count_starwars_sexes(), expected_occurrences)
})

test_that(
  paste(
    "count_column_ocurrences gives correct sex counts",
    "for the starwars dataset"
  ),
  {
    expected_occurrences <- tibble::tibble(
      sex = c("female", "hermaphroditic", "male", "none", NA),
      n = c(16, 1, 60, 6, 4)
    )
    expect_equal(
      count_column_occurrences(dplyr::starwars, sex),
      expected_occurrences
    )
  }
)

test_that(
  paste(
    "count_column_ocurrences gives correct eye_color counts",
    "for the starwars dataset"
  ),
  {
    expected_occurrences <- tibble::tibble(
      eye_color = c(
        "black", "blue", "blue-gray", "brown", "dark", "gold",
        "green, yellow", "hazel", "orange", "pink", "red",
        "red, blue", "unknown", "white", "yellow"
      ),
      n = c(10, 19, 1, 21, 1, 1, 1, 3, 8, 1, 5, 1, 3, 1, 11)
    )

    expect_equal(
      count_column_occurrences(dplyr::starwars, eye_color),
      expected_occurrences
    )
  }
)
