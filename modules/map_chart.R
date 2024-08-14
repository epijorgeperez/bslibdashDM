map_chart_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), width = "100%", height = "680px")
}


map_chart_server <- function(id, metrica, totales_anuales, totales_consultas, totales_incap, totales_hosp, totales_mortalidad, totales_incidencia, citiesmx, data_censo_maestro, anio) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      # Initialize data as NULL
      data <- NULL

      # Get the corresponding data based on the selected metric
      if (metrica() == "Prevalencia") {
        data <- totales_anuales()
      } else if (metrica() == "Consultas") {
        data <- totales_consultas()
      } else if (metrica() == "Incapacidades") {
        data <- totales_incap()
      } else if (metrica() == "Hospitalizaciones") {
        data <- totales_hosp()
      } else if (metrica() == "Mortalidad") {
        data <- totales_mortalidad()
      } else if (metrica() == "Incidencia") {
        data <- totales_incidencia()
      }

      data %>%
        left_join(data_censo_maestro() %>% select(DenominacionUnidad, Localidad), by = c("Nombre_Unidad" = "DenominacionUnidad")) %>%
        left_join(citiesmx(), by = c("Localidad" = "city"))
    })

    output$map <- renderLeaflet({
      data_filtered <- data() %>% filter(Anio == anio())

      # Calculate min and max for Dato column for normalization
      min_val <- min(data_filtered$Dato, na.rm = TRUE)
      max_val <- max(data_filtered$Dato, na.rm = TRUE)

      # Define the desired range for circle sizes
      size_range <- c(50000, 150000) # Adjust this range as needed

      # Normalize the Dato values to the size range for circle radius
      normalized_sizes <- scales::rescale(data_filtered$Dato, to = size_range, from = range(data_filtered$Dato, na.rm = TRUE))

      
      # Initialize a leaflet map
      leaflet(data_filtered) %>%
       addTiles() %>%
       setView(lng = -102.552784, lat = 23.634501, zoom = 5) %>%
       addCircles(
        lng = ~lon, lat = ~lat, weight = 1,
        radius = ~normalized_sizes,  # Use normalized sizes for circle radius
        color = "#0078A8", fillColor = "#0078A8",
        fillOpacity = 0.5, opacity = 1,
        popup = ~paste0(metrica(), ": ", Dato) )%>%
       addProviderTiles(providers$CartoDB.Positron)  # Optionally, add a different map theme
    })
  })
}

