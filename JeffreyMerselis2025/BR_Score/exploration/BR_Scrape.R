# command + shift + S

library(tidyverse)
library(rvest)

# Old code for getting the 2025 season
# url <- "https://www.basketball-reference.com/leagues/NBA_2025_per_game.html"
# page <- read_html(url)
# tables <- page |> html_table(fill = TRUE)
# nba_df <- tables[[1]]
# nba25_df <- nba_df |> mutate(Season = 2025)

scrape_season <- function(season) {
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", season, "_per_game.html")
  page <- read_html(url)
  table <- page |>
    html_node("table") |>
    html_table(fill = TRUE)
  table <- table |>
    mutate(Season = season)
  return(table)
}

all_seasons_data <- data.frame()

# range of seasons scraped (1980 is when ALL the stats are finally tracked I think)
# 1980 to 2024 takes about 4 mins
seasons <- 1980:2024

for (season in seasons) {
  Sys.sleep(5)  # wait 5 seconds I got limited 
  season_data <- scrape_season(season)
  all_seasons_data <- bind_rows(all_seasons_data, season_data)
}















