library(shiny)
library(readr)
library(dplyr)
library(stringr)



# Define UI
ui <- fluidPage(
  titlePanel("Advisee List Processor"),
  
  uiOutput("column_selector"),
  
  # File upload input
  fileInput("file", "Upload CSV File", accept = ".csv"),

  # Dropdown menu for advisor name
  selectInput("advisor_name", "Select Advisor:", choices = NULL),
  
  # Check box for advisor type
  #checkboxGroupInput("advisor_type", "Select Advisor Type(s):", choices = NULL),
  
  # Table to display filtered results
  tableOutput("table"),
  
  # Download button for email list
  downloadButton("download_emails", "Download Email List")
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to load the data from the uploaded CSV
  # Extract and clean individual advisor names
  data_reactive <- reactive({
    req(input$file)  # Ensure a file is uploaded
    read_csv(input$file$datapath) |>
    separate_rows(`Advisor(s)`, sep = "\\),\\s*") %>%  # Split multiple advisors into separate rows
      distinct()  # Remove duplicate rows
  })
  
 
# checkboxes
  
  #  column selector based on uploaded file
  output$column_selector <- renderUI({
    req(data_reactive())  # Ensure data exists
    colnames <- names(data_reactive())
    checkboxGroupInput("selected_columns", 
                       "Select Desired Output:", 
                       choices = colnames)
  })
  
  #  dropdown menu with unique advisor names when the file is uploaded
  observe({
    data <- data_reactive()
    updateSelectInput(session, "advisor_name", choices = unique(data$`Advisor(s)`), selected = unique(data$`Advisor(s)`)[1])
  })
  

  
  

  
  # Reactive expression to filter the data based on selected advisor
   filtered_data <- reactive({
    req(input$advisor_name)  # Ensure an advisor is selected
    filter(`Advisor(s)` == input$advisor_name & `Advisor Type` %in% input$advisor_type)
    data <- data_reactive() %>% filter(`Advisor(s)` == input$advisor_name)
    return(data)
  })
  
  # Render table based on selected columns and advisor filter
  output$table <- renderTable({
    req(input$selected_columns)  # Ensure checkboxes are selected
    filtered_data()[, input$selected_columns, drop = FALSE]
  })
  
  # for filtered email list
  output$download_emails <- downloadHandler(
    filename = function() {
      paste("email_list_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      emails <- filtered_data()[, "Email"]
      write_csv(data.frame(Email = emails), file, row.names = FALSE)
    }
  )
}

# Run 
shinyApp(ui = ui, server = server)




# Define UI
ui <- fluidPage(
  titlePanel("Advisee List Processor"),
  
  uiOutput("column_selector"),
  
  # File upload input
  fileInput("file", "Upload CSV File", accept = ".csv"),
  
  # Dropdown menu for advisor selection
  selectInput("advisor_name", "Select Advisor:", choices = NULL),
  
  # Table to display filtered results
  tableOutput("table"),
  
  # Download button for email list
  downloadButton("download_emails", "Download Email List")
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to load the data from the uploaded CSV
  # Extract and clean individual advisor names
  data_reactive <- reactive({
    req(input$file)  # Ensure a file is uploaded
    read_csv(input$file$datapath) |>
      separate_rows(Advisor(s), sep = "\\),\\s*") %>%  # Split multiple advisors into separate rows
      distinct()  # Remove duplicate rows
  })
  
  
  
  
  #  column selector based on uploaded file
  output$column_selector <- renderUI({
    req(data_reactive())  # Ensure data exists
    colnames <- names(data_reactive())
    checkboxGroupInput("selected_columns", 
                       "Select Desired Output:", 
                       choices = colnames)
  })
  
  #  dropdown menu with unique advisor names when the file is uploaded
  observe({
    data <- data_reactive()
    updateSelectInput(session, "advisor_name", choices = unique(data$Advisor(s)), selected = unique(data$Advisor(s))[1])
  })
  
  # Reactive expression to filter the data based on selected advisor
  filtered_data <- reactive({
    req(input$advisor_name)  # Ensure an advisor is selected
    data <- data_reactive() %>% filter(Advisor(s) == input$advisor_name)
    return(data)
  })
  
  # Render table based on selected columns and advisor filter
  output$table <- renderTable({
    req(input$selected_columns)  # Ensure checkboxes are selected
    filtered_data()[, input$selected_columns, drop = FALSE]
  })
  
  # for filtered email list
  output$download_emails <- downloadHandler(
    filename = function() {
      paste("email_list_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      emails <- filtered_data()[, "Email"]
      write_csv(data.frame(Email = emails), file, row.names = FALSE)
    }
  )
}

# Run 
shinyApp(ui = ui, server = server)
