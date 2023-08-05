library(shiny)
library(dplyr)

# Define the user interface
ui <- fluidPage(
  titlePanel("T-Test Application"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Import Database (CSV format)"),
      selectInput("testType", "Select Test Type",
                  choices = c("One-sample t-test", "Independent t-test", "Paired t-test")),
      selectInput("var1", "Variable 1", choices = NULL),
      selectInput("var2", "Variable 2 (only for Independent t-test)", choices = NULL),
      actionButton("runTest", "Run Test"),
      downloadButton("downloadResults", "Download Results")
    ),
    mainPanel(
      tableOutput("results")
    )
  )
)

# Define the server
server <- function(input, output) {

  # Load the database
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Update the variable choices based on the selected test type
  observeEvent(input$testType, {
    req(input$file)

    if (input$testType == "One-sample t-test" || input$testType == "Paired t-test") {
      updateSelectInput(session, "var1", choices = names(data()))
    }

    if (input$testType == "Independent t-test" || input$testType == "Paired t-test") {
      updateSelectInput(session, "var2", choices = names(data()))
    }
  })

  # Run the selected test
  observeEvent(input$runTest, {
    req(input$var1)

    variable1 <- sym(input$var1)

    if (input$testType == "One-sample t-test") {
      results <- data() %>%
        summarise(
          Mean = mean(!!variable1),
          SD = sd(!!variable1),
          N = n()
        )
    } else if (input$testType == "Independent t-test") {
      req(input$var2)
      variable2 <- sym(input$var2)

      results <- data() %>%
        summarise(
          Mean1 = mean(!!variable1),
          Mean2 = mean(!!variable2),
          SD1 = sd(!!variable1),
          SD2 = sd(!!variable2),
          N1 = n(),
          N2 = n()
        )
    } else if (input$testType == "Paired t-test") {
      req(input$var2)
      variable2 <- sym(input$var2)

      results <- data() %>%
        summarise(
          MeanDiff = mean(!!variable1 - !!variable2),
          SDDiff = sd(!!variable1 - !!variable2),
          N = n()
        )
    }

    output$results <- renderTable(results)
  })

  # Download the results as a CSV file
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("t-test_results", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(results, file, row.names = FALSE)
    }
  )
}

# Launch the Shiny application
shinyApp(ui = ui, server = server)