# Step 1: Authentication
googlesheets4::gs4_auth()

# PART 1: sources_trade FOR TRADE

# Step 2: Rest of Program
# Read a Google Sheet after authentication
sheet_url <- "1UdwgS87x5OsLjNuKaY3JA01GoI5nwsenz62JXCeq0GQ"
sources_trade <- googlesheets4::read_sheet(
  sheet_url,
  sheet = "Final_Sources_Trade"
)

sources_trade <- dplyr::filter(sources_trade, is.na(Trade) == 0)

# Expand data into individual years
expanded_sources_trade <- do.call(rbind, lapply(1:nrow(sources_trade), function(i) {
  data.frame(
    Number = sources_trade$No[i],
    Added_By = sources_trade$Added_By[i],
    APA_Reference = sources_trade$APA_Reference[i],
    Name = sources_trade$Name[i],
    Reporter = sources_trade$Reporter[i],
    Trade = sources_trade$Trade[i],
    ImpExp = sources_trade$`Imp/Exp`[i],
    Measurement = sources_trade$Measurement[i],
    Doc_Format = sources_trade$Doc_Format[i],
    Quality = sources_trade$Quality[i],
    Info_Format = sources_trade$Info_Format[i],
    Scanned = sources_trade$Scanned[i],
    External_Link = sources_trade$External_link[i],
    Saco_Link = sources_trade$SACO_link[i],
    In_Saco = ifelse(is.na(sources_trade$SACO_link[i]) == 0, 1, 0),
    Notes = sources_trade$Notes[i],
    Year = seq(
      sources_trade$Timeline_Start[i],
      sources_trade$Timeline_End[i],
      sources_trade$Timeline_Freq[i]
    )
  )
}))

expanded_sources_trade <- expanded_sources_trade |>
  dplyr::mutate(
    Name = dplyr::if_else(
      Info_Format == "year", paste(Name, Year, sep = "_"), Name
    )
  )

# Plot showing years covered by sources_trade
plot_sources_trade <- ggplot2::ggplot(
  expanded_sources_trade,
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
  expanded_sources_trade,
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
  expanded_sources_trade,
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
  expanded_sources_trade,
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
  expanded_sources_trade,
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


# PART 2: sources_trade FOR PRODUCTION

# Step 3: Rest of Program
sources_prod <- googlesheets4::read_sheet(
  sheet_url,
  sheet = "Final_Sources_Production"
)
sources_prod <- dplyr::filter(sources_prod, is.na(Name) == 0)
