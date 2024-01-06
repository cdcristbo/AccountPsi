# Instala e inicia Shiny si no lo has hecho
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

# Carga el paquete shiny
library(shiny)
library(shinythemes)  # Se incluye el paquete shinythemes
library(readr)
library(dplyr)

# Define la base de datos inicial de pagos
pagos <- data.frame(
  FECHA = as.Date(character(), format = "%Y-%m-%d"),
  ID = integer(),
  NOMBRE = character(),
  DIA_DEL_COBRO = as.Date(character(), format = "%Y-%m-%d"),
  DIA_DEL_PAGO = as.Date(character(), format = "%Y-%m-%d"),
  VALOR_CONSULTA = integer(),
  ABONO = integer(),
  MONEDA = character(),
  PAGO = integer(),
  OBSERVACIONES = character(),
  FORMA_DE_PAGO = character(),
  stringsAsFactors = FALSE
)

# Define la base de datos inicial de pacientes
pacientes <- data.frame(
  ID = integer(),
  NOMBRE = character(),
  PAIS = character(),
  CIUDAD = character(),
  CEL = integer(),
  ESTADO = character(),
  TERAPIA = character(),
  FRECUENCIA = character(),
  stringsAsFactors = FALSE
)

# Ruta del archivo CSV para almacenar los datos
archivo_csv_pagos <- "pagos.csv"
archivo_csv_pacientes <- "pacientes.csv"

# Si el archivo existe, carga los datos
if (file.exists(archivo_csv_pagos)) {
  pagos <- read_csv("pagos.csv", col_types = cols(FECHA = col_character(), 
                                                  DIA_DEL_COBRO = col_character(), 
                                                  DIA_DEL_PAGO = col_character()))
}

if (file.exists(archivo_csv_pacientes)) {
  pacientes <- read.csv(archivo_csv_pacientes, stringsAsFactors = FALSE)
}

# Define el tema "cerulean" de shinythemes para un estilo más atractivo
theme <- shinytheme("cerulean")

ui <- fluidPage(
  titlePanel("Gestión de Pagos y Pacientes", windowTitle = "Gestión de Pagos"),
  theme = theme,
  sidebarLayout(
    sidebarPanel(
      # Panel de Pagos
      tabsetPanel(
        tabPanel("Paciente nuevo",
                 textInput("idPaciente", "ID (Cedula):", ""),
                 textInput("nombrePaciente", "Nombre:", ""),
                 selectInput("estado", "Estado:", c("", "Activo", "Inactivo", "Pausa"), selected = ""),
                 selectInput("pais", "País:", c("Colombia", "Otro")),
                 conditionalPanel(
                   condition = "input.pais == 'Otro'",
                   textInput("otroPais", "Especifique otro país:", "")
                 ),
                 textInput("ciudad", "Ciudad:", ""),
                 numericInput("cel", "Contacto:", value = NULL),
                 selectInput("terapia", "Terapia:", c("", "Individual", "Pareja"), selected = ""),
                 textInput("frecuencia", "Frecuencia:", ""),
                 actionButton("registrarPaciente", "Registrar Paciente")
        ),
        tabPanel("Consulta nueva",
                 dateInput("fechaConsulta", "Fecha de Consulta:", Sys.Date(), format = "yyyy-mm-dd"),
                 selectizeInput("id", "ID (Cedula):", choices = NULL, multiple = FALSE, options = list(placeholder = 'Seleccione un paciente...')),
                 textInput("nombre", "Nombre:", ""),
                 dateInput("diaCobro", "Día del Cobro:", Sys.Date(), format = "yyyy-mm-dd"),
                 dateInput("diaPago", "Día del Pago:", Sys.Date(), format = "yyyy-mm-dd"),
                 numericInput("valorConsulta", "Valor Consulta:", value = NULL),
                 numericInput("abono", "Abono:", value = NULL),
                 selectInput("moneda", "Moneda:", c("", "Dolar", "Pesos"), selected = ""),
                 numericInput("pago", "Pago:", value = NULL),
                 textInput("observaciones", "Observaciones:", ""),
                 selectInput("formaPago", "Forma de Pago:", c("", "Efectivo", "Credito"), selected = ""),
                 actionButton("registrarPago", "Registrar Pago")
        )
        # Panel de Pacientes
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Información del Paciente",
                 fluidRow(
                   column(6, textInput("cedulaHistorico", "Cédula del Paciente:", "")),
                   column(6, actionButton("mostrarHistorico", "Mostrar Historial"))
                 ),
                 fluidRow(
                   column(12, tableOutput("historicoTable"))
                 )
        ),
        
        tabPanel("Resumen Financiero",
                 fluidRow(
                   column(12, tableOutput("pagosTable")),
                   column(12, tableOutput("pacientesTable")),
                   column(12, textOutput("mensajeAlerta"))
                 )
        )
      )
    )
  )
)

# Define el servidor
server <- function(input, output, session) {
  # Almacena la información de los pagos
  pagos_data <- reactiveVal(pagos)
  
  # Almacena la información de los pacientes
  pacientes_data <- reactiveVal(pacientes)
  
  # Observador para cargar datos de pacientes
  observe({
    if (file.exists(archivo_csv_pacientes)) {
      pacientes_data(read.csv(archivo_csv_pacientes, stringsAsFactors = FALSE))
    }
  })
  
  # Actualizar los nombres de los pacientes en el selectizeInput
  observe({
    # Aquí se supone que tus pacientes están cargados en 'pacientes_data()'
    nombres_pacientes <- pacientes_data()$NOMBRE
    # Actualizar 'selectizeInput' con los nombres de los pacientes
    updateSelectizeInput(session, 'id', choices = nombres_pacientes)
  })
  
  # Observador para el cambio de ID (Cedula) seleccionado en Consulta nueva
  observe({
    # Obtener el ID (Cedula) seleccionado
    id_seleccionado_consulta <- input$id
    
    # Actualizar 'selectizeInput' con los IDs de los pacientes
    updateSelectizeInput(session, "id", choices = pacientes_data()$ID, selected = id_seleccionado_consulta)
    
    # Buscar el nombre correspondiente al ID (Cedula) seleccionado en la base de datos de pacientes
    nombre_seleccionado <- pacientes_data() %>%
      filter(ID == as.integer(id_seleccionado_consulta)) %>%
      pull(NOMBRE)
    
    # Actualizar el campo de Nombre con el nombre correspondiente
    updateTextInput(session, "nombre", value = nombre_seleccionado)
  })
  
  
  # Observador para el botón de registrar pagos
  observeEvent(input$registrarPago, {
    # Lógica para manejar la selección de moneda
    if (input$moneda == "Otro") {
      nueva_moneda <- input$otraMoneda
    } else {
      nueva_moneda <- input$moneda
    }
    
    # Crear nuevo pago con la información proporcionada
    nuevo_pago <- data.frame(
      FECHA = as.Date(input$fechaConsulta),
      ID = as.integer(input$id),
      NOMBRE = input$nombre,
      DIA_DEL_COBRO = as.Date(input$diaCobro),
      DIA_DEL_PAGO = as.Date(input$diaPago),
      VALOR_CONSULTA = as.integer(input$valorConsulta),
      ABONO = as.integer(input$abono),
      MONEDA = nueva_moneda,
      PAGO = as.integer(input$pago),
      OBSERVACIONES = input$observaciones,
      FORMA_DE_PAGO = input$formaPago,
      stringsAsFactors = FALSE
    )
    
    # Combinar el nuevo pago con los datos existentes
    pagos_data(rbind(pagos_data(), nuevo_pago))
    
    # Restablecer campos después de registrar
    updateDateInput(session, "fechaConsulta", value = Sys.Date())
    updateTextInput(session, "id", value = "")
    updateSelectizeInput(session, "nombre", selected = "")
    updateDateInput(session, "diaCobro", value = Sys.Date())
    updateDateInput(session, "diaPago", value = Sys.Date())
    updateNumericInput(session, "valorConsulta", value = NULL)
    updateNumericInput(session, "abono", value = NULL)
    updateSelectInput(session, "moneda", selected = "")
    updateNumericInput(session, "pago", value = NULL)
    updateTextInput(session, "observaciones", value = "")
    updateSelectInput(session, "formaPago", selected = "")
    
    # Guarda los datos en el archivo CSV de pagos
    write.csv(pagos_data(), archivo_csv_pagos, row.names = FALSE)
    
    # Limpiar el mensaje de advertencia
    output$mensajeAlerta <- renderText({
      ""
    })
  })
  
  # Observador para el botón de registrar pacientes
  observeEvent(input$registrarPaciente, {
    # Lógica para manejar la selección de país
    if (input$pais == "Otro") {
      nuevo_pais <- input$otroPais
    } else {
      nuevo_pais <- input$pais
    }
    
    # Validar que el ID del paciente no exista
    if (as.integer(input$idPaciente) %in% pacientes_data()$ID) {
      output$mensajeAlerta <- renderText({
        paste("El ID (Cedula) ", input$idPaciente, " ya existe. Por favor, ingrese un ID único.")
      })
      return()
    }
    
    # Validar que el ID del paciente no esté vacío
    if (is.null(input$idPaciente) || input$idPaciente == "") {
      output$mensajeAlerta <- renderText({
        "El campo 'ID (Cedula)' no puede estar vacío. Por favor, ingrese un ID para el paciente."
      })
      return()
    }
    
    # Crear nuevo paciente con la información proporcionada
    nuevo_paciente <- data.frame(
      ID = as.integer(input$idPaciente),
      NOMBRE = input$nombrePaciente,
      PAIS = nuevo_pais,
      CIUDAD = input$ciudad,
      CEL = as.integer(input$cel),
      ESTADO = input$estado,
      TERAPIA = input$terapia,
      FRECUENCIA = input$frecuencia,
      stringsAsFactors = FALSE
    )
    
    # Combinar el nuevo paciente con los datos existentes
    pacientes_data(rbind(pacientes_data(), nuevo_paciente))
    
    # Restablecer campos después de registrar
    updateTextInput(session, "idPaciente", value = "")
    updateTextInput(session, "nombrePaciente", value = "")
    updateSelectInput(session, "pais", selected = "")
    updateTextInput(session, "ciudad", value = "")
    updateNumericInput(session, "cel", value = NULL)
    updateSelectInput(session, "estado", selected = "")
    updateTextInput(session, "terapia", value = "")
    updateTextInput(session, "frecuencia", value = "")
    updateTextInput(session, "otroPais", value = "")
    
    # Guarda los datos en el archivo CSV de pacientes
    write.csv(pacientes_data(), archivo_csv_pacientes, row.names = FALSE)
    
    # Limpiar el mensaje de advertencia
    output$mensajeAlerta <- renderText({
      ""
    })
  })
  
  # Nuevo observador para el botón de mostrar historial
  observeEvent(input$mostrarHistorico, {
    # Validar que la cédula del paciente no esté vacía
    if (is.null(input$cedulaHistorico) || input$cedulaHistorico == "") {
      output$mensajeAlerta <- renderText({
        "El campo 'Cédula del Paciente' no puede estar vacío. Por favor, ingrese la cédula del paciente."
      })
      return()
    }
    
    # Obtener el historial de pagos del paciente
    historial_pagos <- pagos_data() %>%
      filter(ID == as.integer(input$cedulaHistorico))
    
    # Mostrar el historial en la tabla
    output$historicoTable <- renderTable({
      historial_pagos
    })
    
    # Limpiar el mensaje de advertencia
    output$mensajeAlerta <- renderText({
      ""
    })
  })
  
  # Actualiza la tabla de pagos
  output$pagosTable <- renderTable({
    pagos_data()
  })
  
  # Actualiza la tabla de pacientes
  output$pacientesTable <- renderTable({
    pacientes_data()
  })
}

# Ejecuta la aplicación Shiny
shinyApp(ui, server)