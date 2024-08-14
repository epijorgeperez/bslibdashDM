test_mod_UI <- function(id) {
  ns <- NS(id)
  card_body(
    verbatimTextOutput(ns("data_output"), placeholder = TRUE),
    height = "600px"  # Adjust this value as needed
  )
}

test_mod_server <- function(id, metrica, totales_anuales, totales_consultas, totales_incap, totales_hosp, data_censo, data_consulta, data_hosp, data_incapacidad, data_mortalidad) {
  moduleServer(id, function(input, output, session) {
    output$data_output <- renderPrint({
      # Get the selected metric
      metrica <- metrica()
      print("**MÓDULO DE PRUEBAS** (test_mod.R)")
      print("")
      print("")
      print(paste("Métrica seleccionada:", metrica))

      # Initialize data as NULL
    data <- NULL

    # Get the corresponding data based on the selected metric
    if (metrica == "Prevalencia") {
      data <- data_censo()
   } else if (metrica == "Consultas") {
      data <- data_consulta()
   } else if (metrica == "Incapacidades") {
      data <- data_incapacidad()
    } else if (metrica == "Hospitalizaciones") {
      data <- data_hosp()
    } else if (metrica == "Mortalidad") {
      data <- data_mortalidad()
    }

      # Return the first line of the selected data
      print(data)
    })
  })
}
