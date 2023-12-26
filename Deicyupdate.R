# Instala e inicia Shiny si no lo has hecho
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

# Carga el paquete shiny
library(shiny)
library(dplyr)

# Define la base de datos inicial de pagos
pagos <- data.frame(
  FECHA = as.Date(character()),
  ID = integer(),
  NOMBRE = character(),
  DIA_DEL_COBRO = as.Date(character()),
  DIA_DEL_PAGO = as.Date(character()),
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
  pagos <- read.csv(archivo_csv_pagos, stringsAsFactors = FALSE)
}

if (file.exists(archivo_csv_pacientes)) {
  pacientes <- read.csv(archivo_csv_pacientes, stringsAsFactors = FALSE)
}

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Gestión de Pagos y Pacientes"),
  sidebarLayout(
    sidebarPanel(
      # Panel de Pagos
      tabsetPanel(
        tabPanel("Pagos",
                 dateInput("fechaConsulta", "Fecha de Consulta:", Sys.Date()),
                 textInput("id", "ID (Cedula):", ""),
                 # En el UI
                 selectizeInput("nombre", "Nombre:", choices = NULL, multiple = FALSE, options = list(placeholder = 'Seleccione un paciente...')),
                 dateInput("diaCobro", "Día del Cobro:", Sys.Date()),
                 dateInput("diaPago", "Día del Pago:", Sys.Date()),
                 numericInput("valorConsulta", "Valor Consulta:", value = NULL),
                 numericInput("abono", "Abono:", value = NULL),
                 selectInput("moneda", "Moneda:", c("", "Dolar", "Pesos"), selected = ""),
                 numericInput("pago", "Pago:", value = NULL),
                 textInput("observaciones", "Observaciones:", ""),
                 selectInput("formaPago", "Forma de Pago:", c("", "Efectivo", "Credito"), selected = ""),
                 actionButton("registrarPago", "Registrar Pago")
        ),
        # Panel de Pacientes
        tabPanel("Pacientes",
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
        )
      )
    ),
    mainPanel(
      tableOutput("pagosTable"),
      tableOutput("pacientesTable"),
      textOutput("mensajeAlerta")
    )
  )
)

# Define el servidor
server <- function(input, output, session) {
  # Almacena la información de los pagos
  pagos_data <- reactiveVal(pagos)
  
  # Almacena la información de los pacientes
  pacientes_data <- reactiveVal(pacientes)
  
  # Actualizar los nombres de los pacientes en el selectizeInput
  observe({
    # Aquí se supone que tus pacientes están cargados en 'pacientes_data()'
    nombres_pacientes <- pacientes_data()$NOMBRE
    # Actualizar 'selectizeInput' con los nombres de los pacientes
    updateSelectizeInput(session, 'nombre', choices = nombres_pacientes)
  })
  
  # Observador para el botón de registrar pagos
  observeEvent(input$registrarPago, {
    # Verificar si el paciente existe
    paciente_existente <- pacientes_data() %>%
      filter(ID == as.integer(input$id))
    
    if (nrow(paciente_existente) == 0) {
      # Paciente no existe, mostrar mensaje de advertencia
      output$mensajeAlerta <- renderText({
        "No se puede registrar el pago. El paciente no existe en la base de datos."
      })
    } else {
      # Crear nueva información de pago con la información proporcionada
      nuevo_pago <- data.frame(
        FECHA = Sys.Date(),
        ID = as.integer(input$id),
        NOMBRE = input$nombre,
        DIA_DEL_COBRO = as.Date(input$diaCobro),
        DIA_DEL_PAGO = as.Date(input$diaPago),
        VALOR_CONSULTA = as.integer(input$valorConsulta),
        ABONO = as.integer(input$abono),
        MONEDA = input$moneda,
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
      updateTextInput(session, "nombre", value = "")
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
    }
  })
  
  # Observador para el botón de registrar pacientes
  observeEvent(input$registrarPaciente, {
    # Lógica para manejar la selección de país
    if (input$pais == "Otro") {
      nuevo_pais <- input$otroPais
    } else {
      nuevo_pais <- input$pais
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
