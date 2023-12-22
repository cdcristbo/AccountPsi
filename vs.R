library(shiny)
library(shinythemes)
library(shinydashboard)
library(readr);library(readxl);library(openxlsx);library(haven)
library(data.table);library(scales);library(lattice)
library(ggplot2);library(plot3D);library(plotly)
library(reshape);library(reshape2)
library(tidyverse);library(dplyr)
library(forecast);library(dynlm)
library(aTSA);library(olsrr)
library(Rmisc);library(moderndive)
library(magrittr);library(stringr)
library(lubridate);library(stringi)
library(lmtest);library(sandwich)
library(xtable);library(ggExtra)
library(formattable)
library(DT)
 

database <- read_excel("CUENTAS POLPER FINAL.xlsx", sheet="CONSOLIDADO_2023")

nom1 <-  c("ID","NOMBRE","PAIS","CIUDAD","CEL","TERAPIA","FRECUENCIA")
nom2 <-  c("FECHA","ESTADO","DIA DEL COBRO","DIA DEL PAGO","VALOR_CONSULTA","ABONO","MONEDA","PAGO","OBSERVACIONES","FORMA DE PAGO")
database <- database %>%
  mutate(ABONO=as.numeric(ABONO),FECHA=as.Date(FECHA))
# suma del total generado cada dia en cada moneda
db.agg   <- database %>% 
  group_by(FECHA,MONEDA) %>% 
                         dplyr::summarise(TOTAL=sum(VALOR_CONSULTA,na.rm = T))
# suma del abono y respectivo total de consultas+ultima consulta
db.cuentas <- database %>%
  group_by(ID) %>% 
                           dplyr::summarise(TOTAL=sum(VALOR_CONSULTA,na.rm = T),
                           ABONO=sum(ABONO,na.rm = T))
# estima el saldo total
# revisar por que 
db.saldo <- db.cuentas %>% 
  mutate(SALDO=ABONO-TOTAL) 

#nom1 definido al principio
db.nom   <- database %>% 
  dplyr::select(nom1) %>%
  dplyr::distinct()

# adjunta la info del paciente
db.cue   <- db.nom %>%
  left_join(db.saldo,by="ID") 

# grupo deudores saldo negativo
db.deu   <- db.cue %>% 
  dplyr::filter(SALDO<0) %>% 
  arrange(NOMBRE)

# grupo deudores saldo postivo No deben
db.ald   <- db.cue %>% 
  dplyr::filter(SALDO>=0) %>% 
  arrange(NOMBRE) 

datosCont <- function(){
  sidebarLayout(
      textInput("personID", "Digite ID:", ""),
      actionButton("searchButton", "Buscar")
  )}
# datosFech <- function() {
#   sidebarLayout(
#     textInput("personID", "Digite ID:", ""),
#     actionButton("searchButton", "Buscar")
#   )
# }
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
    tabsetPanel(type = "tabs",
                tabPanel("Registrar Usuario",
                  sidebarPanel(
                  textInput("name", "Enter Your Name:", ""),
                  numericInput("age", "Enter Your Age:", value = NULL),
                  actionButton("submit", "Submit"),
                  downloadButton("export", "Export to Excel")
                )
                ),          
      tabPanel("Informacion individual",
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
      tabPanel("Agregado mensual",
               br(),
               # p("Digite el mes que desea consultar:"),
               dataTableOutput("vfecha")
               ),
      tabPanel("Personas al dia",
               br(),
               # p("Digite el mes que desea consultar:"),
               dataTableOutput("valdia")
               ),
      tabPanel("Personas por pagar",
               br(),
               # p("Digite el mes que desea consultar:"),
               dataTableOutput("vporpagar")
               )
    )
  )
# Define the server
server <- function(input, output) {
  output$view1 <- renderDataTable({
    ID.i <- input$personID
    if (ID.i %in% db.cue$ID) {
      consulta = db.cue %>%
        dplyr::filter(ID==ID.i) %>% 
        dplyr::distinct()
      consulta
    } else {
    consulta <- data.frame(Resultado="Persona no inscrita")
    }
  })
  output$view2 <- renderDataTable({
    ID.i <- input$personID
    if (ID.i %in% database$ID) {
        consulta1 = database %>% dplyr::filter(ID==ID.i) %>% 
                                 dplyr::select(nom2)
      consulta1
    } else {
      consulta1 <- data.frame(Resultado="Persona no inscrita")
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
  
  # observeEvent(input$searchButton, {
  #   ID.i <- input$personID
  #   if (ID.i %in% database$ID) {
  #     consulta1 = database %>% dplyr::filter(ID==ID.i) %>% 
  #                              dplyr::select(nom2)
  #     r <- (sum(consulta1$VALOR_CONSULTA,na.rm = T)-sum(consulta1$ABONO,na.rm = T))
  #     color = ifelse(r > 0, "forestgreen", ifelse(r <= 0, "red", "black"))
  #     output$searchResult <- renderText(paste("Saldo",r))
  #   } else {
  #     output$searchResult <- renderText(paste("No hay resultados"))
  #   }
  # })
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

# Run the Shiny app
shinyApp(ui, server)


