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
#' starwars_data <- dplyr::starwars
#' count_column_occurrences(starwars_data, sex)
#' count_column_occurrences(starwars_data, skin_color)
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
#' count_starwars_sexes()
count_starwars_sexes <- function() {
  dplyr::starwars |>
    dplyr::select(sex) |>
    dplyr::count(sex)
}
