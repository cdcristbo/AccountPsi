library(shiny)
library(shinythemes)
library(shinydashboard)
library(readr)
library(ggplot2)
library(plotly)
library(reshape)
library(reshape2)
library(tidyverse)
library(dplyr)
library(forecast)
library(dynlm)
library(aTSA)
library(olsrr)
library(Rmisc)
library(moderndive)
library(magrittr)
library(stringr)
library(lubridate)
library(stringi)
library(lmtest)
library(sandwich)
library(ggExtra)
library(formattable)
library(DT)

pagos <- read_csv("pagos.csv", col_types = cols(
  FECHA = col_date(format = "%m/%d/%Y"),
  DIA_DEL_COBRO = col_datetime(format = "%m/%d/%Y %H:%M"),
  DIA_DEL_PAGO = col_character()
))
pacientes <- read.csv(archivo_csv_pacientes, stringsAsFactors = FALSE)

database = pagos %>%
  left_join(pacientes)

nom1 <- c("ID", "NOMBRE", "PAIS", "CIUDAD", "CEL", "TERAPIA", "FRECUENCIA")
nom2 <- c(
  "FECHA", "ESTADO", "DIA DEL COBRO", "DIA DEL PAGO", "VALOR_CONSULTA",
  "ABONO", "MONEDA", "PAGO", "OBSERVACIONES", "FORMA DE PAGO"
)
database <- database %>% mutate(ABONO = as.numeric(ABONO), FECHA = as.Date(FECHA))
db.agg <- database %>% group_by(FECHA, MONEDA) %>%
  dplyr::summarise(TOTAL = sum(VALOR_CONSULTA, na.rm = TRUE))
db.cuentas <- database %>% group_by(ID) %>%
  dplyr::summarise(
    TOTAL = sum(VALOR_CONSULTA, na.rm = TRUE),
    ABONO = sum(ABONO, na.rm = TRUE)
  )
db.saldo <- db.cuentas %>% mutate(SALDO = ABONO - TOTAL)
db.nom <- database %>% dplyr::select(nom1) %>% dplyr::distinct()
db.cue <- db.nom %>% left_join(db.saldo, by = "ID")
db.deu <- db.cue %>% dplyr::filter(SALDO < 0) %>% arrange(NOMBRE)
db.ald <- db.cue %>% dplyr::filter(SALDO >= 0) %>% arrange(NOMBRE)

datosCont <- function() {
  sidebarLayout(
    textInput("personID", "Digite ID:", ""),
    actionButton("searchButton", "Buscar")
  )}
# Define the UI
ui <- navbarPage(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(70%);
             left: calc(70%);
             }
             ")
    )
  ),
  title = div(strong("POLPER")),
  theme = shinytheme("flatly"),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Informacion individual",
      datosCont(),
      h3("Datos de contacto"),
      dataTableOutput("view1"),
      br(),
      br(),
      h3("Registro historico"),
      dataTableOutput("view2"),
      br(),
      verbatimTextOutput("searchResult")
    ),
    tabPanel(
      "Agregado mensual",
      br(),
      # p("Digite el mes que desea consultar:"),
      dataTableOutput("vfecha")
    ),
    tabPanel(
      "Personas al dia",
      br(),
      # p("Digite el mes que desea consultar:"),
      dataTableOutput("valdia")
    ),
    tabPanel(
      "Personas por pagar",
      br(),
      # p("Digite el mes que desea consultar:"),
      dataTableOutput("vporpagar")
    )
  )
)
server <- function(input, output) {
  output$view1 <- renderDataTable({
    ID.i <- input$personID
    if (ID.i %in% db.cue$ID) {
      consulta = db.cue %>% dplyr::filter(ID == ID.i) %>% dplyr::distinct()
      consulta
    } else {
      consulta <- data.frame(Resultado = "Persona no inscrita")
    }
  })
  output$view2 <- renderDataTable({
    ID.i <- input$personID
    if (ID.i %in% database$ID) {
      consulta1 = database %>% dplyr::filter(ID == ID.i) %>%
        dplyr::select(nom2)
      consulta1
    } else {
      consulta1 <- data.frame(Resultado = "Persona no inscrita")
    }
  })
  output$vfecha <- renderDataTable({
    db.fecha <- db.agg
    # input$dataset
    db.fecha
  })
  
  output$valdia <- renderDataTable({
    db.aldia <- db.ald
    db.aldia
  })
  output$vporpagar <- renderDataTable({
    db.ppag <- db.deu
    db.ppag
  })
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
    })
  
  output$scatter_plot <- renderPlotly({
    plot_ly(data, x = ~Age, y = ~Name, type = 'scatter', mode = 'markers')
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
