library(shiny)
library(writexl)
library(htmlwidgets)
library(plotly)

# Initialize an empty data frame
data <- data.frame(Name = character(0), Age = numeric(0))

ui <- fluidPage(
  titlePanel("Shiny Form Example"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter Your Name:", ""),
      numericInput("age", "Enter Your Age:", value = NULL),
      actionButton("submit", "Submit"),
      downloadButton("export", "Export to Excel")
    ),
    mainPanel(
      plotlyOutput("scatter_plot"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    # Add user input to the data frame
    data <<- rbind(data, data.frame(Name = input$name, Age = input$age))
  })
  
  output$table <- renderTable({
    data
  })
  
  output$export <- downloadHandler(
    filename = function() {
      paste("data_export.xlsx")
    },
    content = function(file) {
      write_xlsx(data, file)
    }
  )
  
  output$scatter_plot <- renderPlotly({
    plot_ly(data, x = ~Age, y = ~Name, type = 'scatter', mode = 'markers')
  })
}

shinyApp(ui, server)

