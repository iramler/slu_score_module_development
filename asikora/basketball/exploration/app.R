library(shiny)
library(tidyverse)
library(readr)
library(here)
library(rvest)

# Load Data
url4 <- "https://herhoopstats.com/stats/ncaa/player/caitlin-clark-stats-11eb2f34-a838-c400-aa81-12df17ae4e1e/"

h4 <- read_html(url4)

tab4 <- h4 |> html_nodes("table")

clark_df <- tab4[[2]] |> html_table(fill = TRUE)

# Clean Data
clark_df$PTS <- gsub(",", "", clark_df$PTS) 
clark_df$PTS <- as.integer(clark_df$PTS)

# Change 'Season' to Class Year FIRST
clark_df1 <- clark_df |>
  mutate(season = case_when(
    season == "2020-21" ~ "First-year",
    season == "2021-22" ~ "Sophomore",
    season == "2022-23" ~ "Junior",
    season == "2023-24" ~ "Senior",
    TRUE ~ season
  )) |> 
  # THEN Select only the desired columns
  select(season, PTS, FGM, FTM, FGA)
# Fix column names
colnames(clark_df1) <- make.names(colnames(clark_df1))

# Add Game Number column for the x-axis
clark_df1$Game_Num <- factor(clark_df1$season, 
                             levels = c("First-year", "Sophomore", "Junior", "Senior"))

# UI ----
ui <- fluidPage(
  titlePanel("Caitlin Clark Game Stats"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stat", "Choose Statistic:", 
                  choices = c("Points" = "PTS", 
                              "Field Goals Made" = "FGM",
                              "Free Throws Made" = "FTM",
                              "Field Goals Attempted" = "FGA")),
      checkboxGroupInput("seasons", "Select Seasons:", 
                         choices = c("First-year", "Sophomore", "Junior", "Senior"),
                         selected = c("First-year", "Sophomore", "Junior", "Senior"))
    ),
    mainPanel(
      plotOutput("statPlot")
    )
  )
) 

# Server -- -
server <- function(input, output) {
  output$statPlot <- renderPlot({
    data <- clark_df1 %>%
      filter(Game_Num %in% input$seasons)  # Filter by selected seasons
    
    ggplot(data, aes(x = Game_Num, y = .data[[input$stat]])) +
      geom_col(fill = "lightblue", color = "black") +
      labs(
        title = paste(input$stat, "Across Selected Seasons"),
        x = "Season",
        y = input$stat
      ) +
      theme_minimal()
  })
}
# Run the App ----
shinyApp(ui = ui, server = server)