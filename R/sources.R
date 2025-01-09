#' Create a new dataframe where each row has a year range into one where each
#' row is a single year, effectively 'expanding' the whole year range
#'
#' @param trade_sources A tibble dataframe
#' where each row contains the year range
#'
#' @return A tibble dataframe where each row
#' corresponds to a single year for a given source
#'
#' @export
#'
#' @examples
#' trade_sources <- tibble::tibble(
#'   Name = c("a", "b", "c"),
#'   Trade = c("t1", "t2", "t3"),
#'   Info_Format = c("year", "partial_series", "year"),
#'   Timeline_Start = c(1, 1, 2),
#'   Timeline_End = c(3, 4, 5),
#'   Timeline_Freq = c(1, 1, 2),
#'   `Imp/Exp` = "Imp",
#'   SACO_link = NA,
#' )
#' expand_trade_sources(trade_sources)
expand_trade_sources <- function(trade_sources) {
  trade_sources |>
    dplyr::filter(is.na(Trade) == 0) |>
    .expand_trade_years() |>
    dplyr::mutate(
      Name = dplyr::if_else(
        Info_Format == "year", paste(Name, Year, sep = "_"), Name
      ),
      ImpExp = `Imp/Exp`,
      In_Saco = as.integer(!is.na(SACO_link)),
    )
}

.expand_trade_years <- function(trade_sources) {
  trade_sources <- dplyr::mutate(trade_sources, No = dplyr::row_number())

  trade_sources |>
    dplyr::group_by(No) |>
    tidyr::expand(Year = seq(Timeline_Start, Timeline_End, Timeline_Freq)) |>
    dplyr::inner_join(trade_sources, by = "No")
}


# Step 1: Authentication
googlesheets4::gs4_auth()
sheet_url <- "1UdwgS87x5OsLjNuKaY3JA01GoI5nwsenz62JXCeq0GQ"

# PART 1: trade_sources FOR TRADE

# Step 2: Rest of Program
expanded_trade_sources <-
  sheet_url |>
  googlesheets4::read_sheet(sheet = "Final_Sources_Trade") |>
  expand_trade_sources()

# Plot showing years covered by sources_trade
plot_sources_trade <- ggplot2::ggplot(
  expanded_trade_sources,
  ggplot2::aes(y = Trade, x = Year, fill = "lightblue")
) +
  ggplot2::geom_tile(alpha = .8) +
  ggplot2::theme_dark() +
  ggplot2::labs(title = "Source Availability by Country") +
  ggplot2::scale_fill_identity() +
  ggplot2::facet_wrap(~Reporter, ncol = 1)
plot_sources_trade

# Plot showing by years, colored by sources_trade
plot_sources_trade_sources <- ggplot2::ggplot(
  expanded_trade_sources,
  ggplot2::aes(y = Trade, x = Year, fill = Name, alpha = .8)
) +
  ggplot2::geom_tile() +
  # theme_minimal() +
  ggplot2::theme(legend.position = "None") +
  ggplot2::labs(title = "Source Availability by Country") +
  ggplot2::facet_grid(ImpExp ~ Reporter, scales = "free", space = "free_x")
plot_sources_trade_sources

# Plot showing years covered by sources_trade, colored by quality
plot_sources_trade_quality <- ggplot2::ggplot(
  expanded_trade_sources,
  ggplot2::aes(y = Trade, x = Year, alpha = .8)
) +
  ggplot2::geom_tile(ggplot2::aes(fill = factor(Quality))) +
  ggplot2::theme_dark() +
  ggplot2::labs(title = "Source Availability by Country", fill = "Quality") +
  ggplot2::guides(alpha = "none") +
  ggplot2::scale_fill_manual(
    values = c("Bad" = "red", "Good" = "green", "Unknown" = "blue")
  ) +
  ggplot2::facet_grid(ImpExp ~ Reporter, scales = "free", space = "free_x")
plot_sources_trade_quality

# Plot showing years covered by sources_trade, colored by scanned/not scanned
plot_sources_trade_scanned <- ggplot2::ggplot(
  expanded_trade_sources,
  ggplot2::aes(y = Trade, x = Year)
) +
  ggplot2::geom_tile(ggplot2::aes(fill = factor(Scanned), alpha = .8)) +
  ggplot2::theme_dark() +
  ggplot2::labs(title = "Scanned sources_trade by Country", fill = "Scanned") +
  ggplot2::scale_fill_manual(
    labels = c("0" = "No", "1" = "Yes"), values = c("0" = "red", "1" = "green")
  ) +
  ggplot2::guides(alpha = "none") +
  ggplot2::facet_grid(ImpExp ~ Reporter, scales = "free", space = "free_x")
plot_sources_trade_scanned

# Plot showing years covered by sources_trade, colored by whether or not in SACO
plot_sources_trade_saco <- ggplot2::ggplot(
  expanded_trade_sources,
  ggplot2::aes(y = Trade, x = Year, alpha = .8)
) +
  ggplot2::geom_tile(ggplot2::aes(fill = factor(In_Saco))) +
  ggplot2::theme_dark() +
  ggplot2::labs(title = "Source Availability by Country", fill = "In Saco") +
  ggplot2::guides(alpha = "none") +
  ggplot2::scale_fill_manual(
    labels = c("0" = "No", "1" = "Yes"), values = c("0" = "red", "1" = "green")
  ) +
  ggplot2::facet_grid(ImpExp ~ Reporter, scales = "free", space = "free_x")
plot_sources_trade_saco
