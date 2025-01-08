#' Count the number of occurrences of each value in the specified column
#'
#' @param df A data.frame from which the column value occurrences are counted
#' @param column A character vector with the column's name that must be counted
#'
#' @return A data.frame with two columns: the column value (with same name)
#' and n, the number of occurrences
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")'
count_column_occurrences <- function(df, column) {
  df |>
    dplyr::select({{column}}) |>
    dplyr::count({{column}})
}

#' Count the number of occurrences of each value in the "sex" column of
#' the dplyr::starwars dataset
#'
#' @return A data.frame with two columns: the column value (with same name)
#' and n, the number of occurrences
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")'
count_starwars_sexes <- function() {
  dplyr::starwars |>
    dplyr::select(sex) |>
    dplyr::count(sex)
}
