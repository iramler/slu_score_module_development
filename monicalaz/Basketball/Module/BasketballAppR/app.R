# === Load Libraries ===
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# === Load and Prepare Data ===
nba_data <- nba_all |>
  mutate(
    PTS = as.numeric(PTS),
    AST = as.numeric(AST),
    TRB = as.numeric(TRB),
    `3P` = as.numeric(`3P`)
  ) |>
  filter(!is.na(PTS), !is.na(AST), !is.na(TRB), !is.na(`3P`))

# === K-means Clustering ===
cluster_input <- nba_data |> select(PTS, AST, TRB, `3P`)
cluster_scaled <- scale(cluster_input)
set.seed(42)
k_result <- kmeans(cluster_scaled, centers = 4, nstart = 20)

# Add cluster back to dataset
nba_data <- nba_data |>
  mutate(Cluster = as.factor(k_result$cluster))

# === UI ===
ui <- fluidPage(
  titlePanel("NBA Player Role Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("era", "Select Era:", choices = unique(nba_data$Era), selected = "2020s"),
      checkboxGroupInput("pos", "Select Position(s):", choices = unique(nba_data$Pos), selected = unique(nba_data$Pos)),
      textInput("search", "Search Player (optional):", value = ""),
      checkboxInput("topScorers", "Filter to PTS > 20", value = FALSE),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Player Table",
                 h4("Filtered Player Data"),
                 DTOutput("playerTable"),
                 br(),
                 h4("Summary Statistics"),
                 verbatimTextOutput("summaryStats")
        ),
        
        tabPanel("Visual Explorer",
                 h4("Boxplot: Points by Cluster"),
                 plotOutput("boxPlot", height = "400px"),
                 h4("Scatterplot: AST vs. PTS"),
                 plotOutput("scatterPlot", height = "400px")
        ),
        
        tabPanel("Cluster Explorer",
                 h4("Cluster Centers (Standardized)"),
                 tableOutput("clusterCenters"),
                 br(),
                 h4("Histogram: PTS by Cluster"),
                 plotOutput("histPlot", height = "400px")
        )
      )
    )
  )
)

# === SERVER ===
server <- function(input, output) {
  
  # Reactive: Filtered Dataset
  filtered_data <- reactive({
    data <- nba_data |>
      filter(Era == input$era)
    
    if (!is.null(input$pos)) {
      data <- data |> filter(Pos %in% input$pos)
    }
    
    if (input$search != "") {
      data <- data |> filter(grepl(input$search, Player, ignore.case = TRUE))
    }
    
    if (input$topScorers) {
      data <- data |> filter(PTS > 20)
    }
    
    data
  })
  
  # === TABLE ===
  output$playerTable <- renderDT({
    datatable(
      filtered_data() |> select(Player, Pos, Era, Cluster, PTS, AST, TRB, `3P`),
      options = list(pageLength = 10)
    )
  })
  
  # === SUMMARY ===
  output$summaryStats <- renderPrint({
    filtered_data() |> summarise(
      avg_pts = round(mean(PTS, na.rm = TRUE), 2),
      avg_ast = round(mean(AST, na.rm = TRUE), 2),
      avg_trb = round(mean(TRB, na.rm = TRUE), 2),
      avg_3p  = round(mean(`3P`, na.rm = TRUE), 2),
      n_players = n()
    )
  })
  
  # === BOXPLOT ===
  output$boxPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Cluster, y = PTS, fill = Cluster)) +
      geom_boxplot() +
      labs(title = "Points per Game by Cluster", x = "Cluster", y = "PTS") +
      theme_minimal()
  })
  
  # === SCATTERPLOT ===
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = AST, y = PTS, color = Cluster)) +
      geom_point(alpha = 0.6) +
      labs(title = "AST vs PTS by Cluster", x = "AST", y = "PTS") +
      theme_minimal()
  })
  
  # === CLUSTER CENTERS ===
  output$clusterCenters <- renderTable({
    as.data.frame(k_result$centers)
  }, rownames = TRUE)
  
  # === HISTOGRAM ===
  output$histPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = PTS, fill = Cluster)) +
      geom_histogram(bins = 30, alpha = 0.7, color = "white") +
      labs(title = "Distribution of PTS by Cluster", x = "PTS", y = "Count") +
      theme_minimal()
  })
  
  # === DOWNLOAD ===
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("filtered_nba_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# === Run App ===
shinyApp(ui = ui, server = server)