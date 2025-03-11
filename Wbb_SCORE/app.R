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
clark_df$PTS <- gsub(",", "", clark_df$PTS) # Remove commas
clark_df$PTS <- as.integer(clark_df$PTS)    # Convert to integer

# Fix column names
colnames(clark_df) <- make.names(colnames(clark_df))

# Add Game Number column for the x-axis
clark_df$Game_Num <- 1:nrow(clark_df)

# UI ----
ui <- fluidPage(
  titlePanel("Caitlin Clark Game Stats"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stat", "Choose Statistic:", 
                  choices = c("Points" = "PTS", 
                              "Assists" = "AST")),
      selectInput("games", "Select Number of Games:", 
                  choices = 4:1, 
                  selected = 4)
    ),
    mainPanel(
      plotOutput("statPlot")
    )
  )
)

# Server ----
server <- function(input, output) {
  output$statPlot <- renderPlot({
    data <- clark_df[1:as.numeric(input$games), ] # Filter games based on selection
    
    ggplot(data, aes(x = Game_Num, y = .data[[input$stat]])) +
      geom_col(fill = "lightblue", color = "black") +
      labs(
        title = paste(input$stat, "Over First", input$games, "Seasons"),
        x = "Season",
        y = input$stat
      ) +
      theme_minimal()
  })
}

# Run the App ----
shinyApp(ui = ui, server = server)