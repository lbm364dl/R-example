#' Create a new dataframe where each row has a year range into one where each
#' row is a single year, effectively 'expanding' the whole year range
#'
#' @param production_sources A tibble dataframe
#' where each row contains the year range
#'
#' @return A tibble dataframe where each row
#' corresponds to a single year for a given source
#'
#' @export
#'
#' @examples
#' production_sources <- tibble::tibble(
#'   Name = c("a", "b", "c"),
#'   Info_Format = c("year", "partial_series", "year"),
#'   Timeline_Start = c(1, 1, 2),
#'   Timeline_End = c(3, 4, 5),
#'   Timeline_Freq = c(1, 1, 2),
#'   SACO_link = NA,
#' )
#' expanded_production_sources(production_sources)
expanded_production_sources <- function(production_sources) {
  non_na_cols <- c("Information_Level", "Timeline_Start", "Timeline_End", "Timeline_Freq")
  production_sources |>
    dplyr::filter(!.any_na_col(non_na_cols)) |>
    .expand_production_years() |>
    dplyr::mutate(
      Name = dplyr::if_else(
        Info_Format == "year", paste(Name, Year, sep = "_"), Name
      ),
      In_Saco = as.integer(!is.na(SACO_link)),
    )
}

.expand_production_years <- function(production_sources) {
  production_sources <- dplyr::mutate(production_sources, No = dplyr::row_number())
  
  production_sources |>
    dplyr::group_by(No) |>
    tidyr::expand(Year = seq(Timeline_Start, Timeline_End, Timeline_Freq)) |>
    dplyr::inner_join(production_sources, by = "No")
}

.any_na_col <- function(cols_to_check) {
  dplyr::if_any(dplyr::all_of(cols_to_check), is.na)
}