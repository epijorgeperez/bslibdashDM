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

# Load required libraries
library(sf)
library(dplyr)

# Define the path to the shapefile
shapefile_path <- "data/mun21gw/mun21gw.shp"

# Load the shapefile
municipalities <- st_read(shapefile_path)

jalisco_shape <- municipalities %>%
  filter(NOM_ENT == "Jalisco")
# Inspect the structure of the shapefile
print(municipalities)       # Overview of the data
glimpse(municipalities)     # Detailed structure of the attributes
plot(st_geometry(jalisco_shape))  # Quick plot of the geometries


data_censo_maestro <- read_csv("data/cuums_maestro.csv")
citiesmx <- read_csv("data/citiesmx.csv")

anio <- 2020


    data <- read_csv("data/tb_censo_DM.csv")
    data <- data %>% 
      filter (Sexo == 0) %>%
      mutate(Nombre_Unidad = ifelse(nchar(Cve_Presupuestal) < 3 & Cve_Presupuestal != "00", "OOAD", Nombre_Unidad)) %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
      summarise(Pacientes_DM = sum(Pacientes_DM, na.rm = TRUE), 
                PAMF = sum(PAMF, na.rm = TRUE),
                Dato = Pacientes_DM / PAMF * 100,
                .groups = 'drop') %>%
      filter(PAMF != 0)
base_data <- data

      
      # Join with location data
base_data <- base_data %>%
        left_join(data_censo_maestro %>% rename("Municipio" ="Municipio o Delegacion")%>%
                   select(DenominacionUnidad, Municipio), 
                 by = c("Nombre_Unidad" = "DenominacionUnidad")) %>%
        left_join(citiesmx, by = c("Municipio" = "city"))
    
    # Aggregate data by municipality
    municipality_data <-
      base_data %>%
        filter(Anio == anio) %>%
        group_by(Municipio) %>%
        summarise(
          valor_total = sum(Dato, na.rm = TRUE),
          valor_promedio = mean(Dato, na.rm = TRUE),
          n_unidades = n()
        )
  
    
    # Join shapefile with aggregated data
    shape_data <-jalisco_shape %>%
        left_join(municipality_data, 
                 by = c("NOM_MUN" = "Municipio"))  
    
    # Create color palette
    pal <-  colorNumeric(
        palette = "YlOrRd",
        domain = shape_data$valor_promedio,
        na.color = "#808080"
      )
  

     
          # Ensure the shapefile is in the correct projection
      shape_data_transformed <- st_transform(shape_data, 4326)
      
      leaflet() %>%
        # Base map
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        # Set view to Jalisco
        setView(lng = -103.3496, lat = 20.6597, zoom = 7) %>%
        
        # Add choropleth layer
        addPolygons(
          data = shape_data_transformed,
          fillColor = ~pal(valor_promedio),
          fillOpacity = 0.7,
          weight = 1,
          color = "#FFFFFF",
          dashArray = "3",
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = ~sprintf(
            "<strong>%s</strong><br/>%s: %.2f<br/>Unidades médicas: %d",
            NOM_MUN,  # Adjust to your shapefile's municipality column name
            "PREVALENCIA",
            valor_promedio,
            n_unidades
          ) %>% lapply(HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        
        # Add medical units as points
        addCircleMarkers(
          data = base_data %>% filter(Anio == anio),
          lng = ~lon, 
          lat = ~lat,
          radius = 4,
          color = "#1f77b4",
          stroke = FALSE,
          fillOpacity = 0.6,
          popup = ~sprintf(
            "<strong>%s</strong><br/>%s: %.2f<br/>Municipio: %s",
            Nombre_Unidad,
            "Prevalencia",
            Dato,
            Municipio
          ) %>% lapply(HTML)
        ) %>%
        
        # Add legend
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = shape_data$valor_promedio,
          title = sprintf("%s por Municipio", "prevalencia", anio),
          opacity = 0.7,
          labFormat = labelFormat(digits = 2)
        )
