library(shiny)
library(ggplot2)
library(readr)

# Define UI
ui <- fluidPage(
  titlePanel("City Distance Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      # ... other input elements ...
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  options(shiny.maxRequestSize = 900*1024^2)  # Set limit to 30MB
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read_csv(inFile$datapath)
  })
  
  output$linePlot <- renderPlot({
    # Make sure data is available
    if (is.null(data())) {
      return(NULL)
    }
    
    # Proceed with your plotting logic using 'data()' ...
  })
}

# Run the app
shinyApp(ui, server)
