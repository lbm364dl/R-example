#Step 1: Install packages, load libraries
library(tidyverse)
library(googlesheets4)
install.packages("ggrepel")
library(ggrepel)

#Step 2: Authentication
gs4_deauth()
gs4_auth()

#Step 3: Rest of Program
# Read a Google Sheet after authentication
sheet_url <- "1UdwgS87x5OsLjNuKaY3JA01GoI5nwsenz62JXCeq0GQ"
data <- read_sheet(sheet_url, sheet = "Final_Sources_Trade")
print(data)

sources <- filter(data, is.na(Trade) == 0)

# Expand data into individual years
expanded_years <- do.call(rbind, lapply(1:nrow(sources), function(i) {
  data.frame(
    Source = sources$Name[i],
    Reporter = sources$Reporter[i],
    Trade = sources$Trade[i],
    Quality = sources$Quality[i],
    Scanned = sources$Scanned[i],
    ImpExp = sources$`Imp/Exp`[i],
    Year = seq(sources$Timeline_Start[i], sources$Timeline_End[i], sources$Timeline_Freq[i])
  )
}))
help(seq)
expanded_years <- expanded_years %>%
  mutate(fcolor = factor(if_else(Quality == "Bad", "red", "green")))

#Plot showing years covered by sources
ggplot(expanded_years, aes(y = Trade, x = Year, fill = "lightblue")) + 
  geom_tile(alpha = .8) +
  theme_dark() +
  labs(title = "Source Availability by Country") +
  scale_fill_identity() +
  facet_wrap(~ Reporter, ncol =  1)

#Plot showing by years, colored by sources
ggplot(expanded_years, aes(y = Trade, x = Year, fill = Source)) + 
  geom_tile(alpha = .8) +
  theme_dark() +
  theme(legend.position = "None") +
  labs(title = "Source Availability by Country") +
  facet_grid(ImpExp~Reporter, scales="free", space="free_x")

#Plot showing years covered by sources, colored by quality
ggplot(expanded_years, aes(y = Trade, x = Year, fill = fcolor, alpha = .8)) + 
  geom_tile() +
  theme_dark() +
  labs(title = "Source Availability by Country", 
       subtitle = "High Quality in Green, Low Quality in Red") +
  theme(legend.position = "none") +
  scale_fill_identity() +
  facet_wrap(~ Reporter, ncol =  1)

#Plot showing years covered by sources, colored by scanned/not scanned
ggplot(expanded_years, aes(y = Trade, x = Year, fill = if_else(Scanned == 0, "red", "green"), alpha = .8)) +
  geom_tile() +
  theme_dark() +
  labs(title = "Scanned Sources by Country", subtitle = "Scanned in Green, Unscanned in Red") +
  scale_fill_identity() +
  theme(legend.position = "none") +
  facet_wrap(~ Reporter, ncol =  1)

glimpse(expanded_years)
