# Instala e inicia Shiny si no lo has hecho
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

# Carga el paquete shiny
library(shiny)

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
archivo_csv <- "pacientes.csv"

# Si el archivo existe, carga los datos
if (file.exists(archivo_csv)) {
  pacientes <- read.csv(archivo_csv, stringsAsFactors = FALSE)
}

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Gestión de Pacientes"),
  sidebarLayout(
    sidebarPanel(
      textInput("id", "ID (Cedula):", ""),
      textInput("nombre", "Nombre:", ""),
      selectInput("pais", "País:", c("Colombia", "Otro")),
      conditionalPanel(
        condition = "input.pais == 'Otro'",
        textInput("otroPais", "Especifique otro país:", "")
      ),
      textInput("ciudad", "Ciudad:", ""),
      numericInput("cel", "Contacto:", value = NULL),
      selectInput("estado", "Estado:", c("", "Activo", "Inactivo", "Pausa"), selected = ""),
      selectInput("terapia", "Terapia:", c("", "Individual", "Pareja"), selected = ""),
      textInput("frecuencia", "Frecuencia:", ""),
      actionButton("registrar", "Registrar Paciente"),
      tabPanel("Buscar Paciente",
               textInput("buscarID", "ID (Cedula):", ""),
               textInput("buscarNombre", "Nombre:", ""),
               actionButton("buscarPaciente", "Buscar Paciente")
      )
    ),
    mainPanel(
      tableOutput("pacientesTable"),
      tabPanel("Información del Paciente",
               tableOutput("infoPacienteTable")
      )
    )
  )
)

# Define el servidor
server <- function(input, output, session) {
  # Almacena la información de los pacientes
  pacientes_data <- reactiveVal(pacientes)
  
  # Observador para el botón de registrar
  observeEvent(input$registrar, {
    # Lógica para manejar la selección de país
    if (input$pais == "Otro") {
      nuevo_pais <- input$otroPais
    } else {
      nuevo_pais <- input$pais
    }
    
    # Crear nuevo paciente con la información proporcionada
    nuevo_paciente <- data.frame(
      ID = as.integer(input$id),
      NOMBRE = input$nombre,
      PAIS = nuevo_pais,
      CIUDAD = input$ciudad,
      CEL = as.integer(input$cel),
      ESTADO = input$estado,
      TERAPIA = input$terapia,
      FRECUENCIA = input$frecuencia,
      stringsAsFactors = FALSE
    )
    
    # Combina el nuevo paciente con la base de datos existente
    pacientes_data(rbind(pacientes_data(), nuevo_paciente))
    
    # Restablecer campos después de registrar
    updateTextInput(session, "id", value = "")
    updateTextInput(session, "nombre", value = "")
    updateSelectInput(session, "pais", selected = "")
    updateTextInput(session, "ciudad", value = "")
    updateNumericInput(session, "cel", value = NULL)
    updateSelectInput(session, "estado", selected = "")
    updateTextInput(session, "terapia", value = "")
    updateTextInput(session, "frecuencia", value = "")
    updateTextInput(session, "otroPais", value = "")
    
    # Guarda los datos en el archivo CSV
    write.csv(pacientes_data(), archivo_csv, row.names = FALSE)
  })
  
  # Actualiza la tabla de pacientes
  output$pacientesTable <- renderTable({
    pacientes_data()
  })
}

# Ejecuta la aplicación Shiny
shinyApp(ui, server)
