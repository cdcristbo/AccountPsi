library(shiny)
library(shinythemes)
library(shinydashboard)
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringi)
library(DT)

pagos <- read_csv("pagos.csv", col_types = cols(FECHA = col_character(), 
                                                DIA_DEL_COBRO = col_character(), 
                                                DIA_DEL_PAGO = col_character()))

pacientes <- read.csv(archivo_csv_pacientes, stringsAsFactors = FALSE)

database = pagos %>%
  left_join(pacientes)

nom1 <- c("ID", "NOMBRE", "PAIS", "CIUDAD", "CEL", "TERAPIA", "FRECUENCIA")
nom2 <- c(
  "FECHA", "ESTADO", "DIA DEL COBRO", "DIA DEL PAGO", "VALOR_CONSULTA",
  "ABONO", "MONEDA", "PAGO", "OBSERVACIONES", "FORMA DE PAGO"
)
database <- database %>% mutate(ABONO = as.numeric(ABONO), FECHA = as.Date(FECHA))
db.agg <- database %>%
  group_by(FECHA, MONEDA) %>%
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
      HTML("
        .navbar-default {
          background-color: #337ab7;
          border-color: #2e6da4;
        }

        .navbar-default .navbar-brand {
          color: #ffffff;
        }

        .navbar-default .navbar-nav > li > a {
          color: #ffffff;
        }

        .navbar-default .navbar-toggle {
          border-color: #337ab7;
        }

        .navbar-default .navbar-toggle:hover,
        .navbar-default .navbar-toggle:focus {
          background-color: #337ab7;
        }

        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:hover,
        .navbar-default .navbar-nav > .active > a:focus {
          background-color: #337ab7;
          color: #ffffff;
        }

        .tab-content {
          background-color: #ffffff;
          border: 1px solid #ddd;
          padding: 10px;
          margin-top: 20px;
        }

        .navbar-default .navbar-nav > li > a:hover,
        .navbar-default .navbar-nav > li > a:focus {
          background-color: #2e6da4;
          color: #ffffff;
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
      sidebarLayout(
        sidebarPanel(
          textInput("personID", "Digite ID:", ""),
          actionButton("searchButton", "Buscar")
        ),
        mainPanel(
          h3("Datos de contacto"),
          dataTableOutput("view1"),
          br(),
          br(),
          h3("Registro historico"),
          dataTableOutput("view2"),
          br(),
          verbatimTextOutput("searchResult")
        )
      )
    ),
    tabPanel(
      "Agregado mensual",
      br(),
      dataTableOutput("vfecha")
    ),
    tabPanel(
      "Personas al dia",
      br(),
      dataTableOutput("valdia")
    ),
    tabPanel(
      "Personas por pagar",
      br(),
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
        dplyr::select(one_of(nom2))  # Utiliza one_of para seleccionar solo las columnas que existen
      consulta1
    } else {
      consulta1 <- data.frame(Resultado = "Persona no inscrita")
    }
  })
  
  output$vfecha <- renderDataTable({
    db.fecha <- db.agg
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
