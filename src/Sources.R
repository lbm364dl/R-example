#Step 1: Install packages, load libraries
library(tidyverse)
library(googlesheets4)


#Step 2: Authentication
gs4_deauth()
gs4_auth()

#PART 1: sources_trade FOR TRADE

#Step 3: Rest of Program
# Read a Google Sheet after authentication
sheet_url <- "1UdwgS87x5OsLjNuKaY3JA01GoI5nwsenz62JXCeq0GQ"
sources_trade <- read_sheet(sheet_url, sheet = "Final_Sources_Trade")
print(data)

sources_trade <- filter(sources_trade, is.na(Trade) == 0)

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
    Year = seq(sources_trade$Timeline_Start[i], sources_trade$Timeline_End[i], sources_trade$Timeline_Freq[i])
  )
}))

expanded_sources_trade <- expanded_sources_trade %>%
  mutate(Name = if_else(Info_Format == "year", paste(Name, Year, sep = "_"), Name))

#Plot showing years covered by sources_trade
plot_sources_trade <- ggplot(expanded_sources_trade, aes(y = Trade, x = Year, fill = "lightblue")) + 
  geom_tile(alpha = .8) +
  theme_dark() +
  labs(title = "Source Availability by Country") +
  scale_fill_identity() +
  facet_wrap(~ Reporter, ncol =  1)
plot_sources_trade

#Plot showing by years, colored by sources_trade
plot_sources_trade_sources <-ggplot(expanded_sources_trade, aes(y = Trade, x = Year, fill = Name, alpha = .8)) + 
  geom_tile() +
  #theme_minimal() +
  theme(legend.position = "None") +
  labs(title = "Source Availability by Country") +
  facet_grid(ImpExp~Reporter, scales="free", space="free_x")
plot_sources_trade_sources

#Plot showing years covered by sources_trade, colored by quality
plot_sources_trade_quality <-ggplot(expanded_sources_trade, aes(y = Trade, x = Year, alpha = .8)) + 
  geom_tile(aes(fill = factor(Quality))) +
  theme_dark() +
  labs(title = "Source Availability by Country", fill = "Quality") +
  guides(alpha = "none") +
  scale_fill_manual(values = c("Bad" = "red", "Good" = "green", "Unknown" = "blue")) +
  facet_grid(ImpExp~Reporter, scales = "free", space = "free_x")
plot_sources_trade_quality

#Plot showing years covered by sources_trade, colored by scanned/not scanned
plot_sources_trade_scanned <- ggplot(expanded_sources_trade, aes(y = Trade, x = Year)) +
  geom_tile(aes(fill = factor(Scanned), alpha = .8)) +
  theme_dark() +
  labs(title = "Scanned sources_trade by Country", fill = "Scanned") +
  scale_fill_manual(labels = c("0" = "No", "1" = "Yes"), values = c("0" = "red", "1" = "green")) +
  guides(alpha = "none") +
  facet_grid(ImpExp~Reporter, scales="free", space="free_x")
plot_sources_trade_scanned

#Plot showing years covered by sources_trade, colored by whether or not in SACO
plot_sources_trade_saco <-ggplot(expanded_sources_trade, aes(y = Trade, x = Year, alpha = .8)) + 
  geom_tile(aes(fill = factor(In_Saco))) +
  theme_dark() +
  labs(title = "Source Availability by Country", fill = "In Saco") +
  guides(alpha = "none") +
  scale_fill_manual(labels = c("0" = "No", "1" = "Yes"), values = c("0" = "red", "1" = "green")) +
  facet_grid(ImpExp~Reporter, scales = "free", space = "free_x")
plot_sources_trade_saco


#PART 2: sources_trade FOR PRODUCTION

#Step 3: Rest of Program
sources_prod <- read_sheet(sheet_url, sheet = "Final_Sources_Production")
sources_prod <- filter(sources_prod, is.na(Name) == 0)
