# modules/map_chart.R

map_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), width = "100%", height = "400px"),
    absolutePanel(
      id = ns("legend"), class = "legend",
      top = 10, right = 10,
      style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);"
    )
  )
}

map_chart_server <- function(id, metrica, totales_anuales, totales_consultas, totales_incap, 
                             totales_hosp, totales_mortalidad, totales_incidencia, 
                             citiesmx, data_censo_maestro, jalisco_shape, anio) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive data based on the selected metric
    data <- reactive({
      req(metrica(), anio(), data_censo_maestro(), citiesmx())
      
      # Get base data based on the selected metric
      base_data <- switch(metrica(),
        "Prevalencia" = totales_anuales(),
        "Consultas" = totales_consultas(),
        "Incapacidades" = totales_incap(),
        "Hospitalizaciones" = totales_hosp(),
        "Mortalidad" = totales_mortalidad(),
        "Incidencia" = totales_incidencia()
      )
      
      req(base_data)
      
      # Join with location data
      base_data %>%
        left_join(data_censo_maestro() %>% 
                   rename("Municipio" = "Municipio o Delegacion") %>%
                   select(DenominacionUnidad, Municipio), 
                 by = c("Nombre_Unidad" = "DenominacionUnidad")) %>%
        left_join(citiesmx(), by = c("Municipio" = "city"))
    })
    
    # Aggregate data by municipality
    municipality_data <- reactive({
      data() %>%
        filter(Anio == anio()) %>%
        group_by(Municipio) %>%
        summarise(
          valor_total = sum(Dato, na.rm = TRUE),
          valor_promedio = mean(Dato, na.rm = TRUE),
          n_unidades = n()
        )
    })
    
    # Join shapefile with aggregated data
    shape_data <- reactive({
      jalisco_shape %>%
        left_join(municipality_data(), 
                  by = c("NOM_MUN" = "Municipio"))  # Adjust "NOM_MUN" to match your shapefile's municipality column name
    })
    
    # Create color palette
    pal <- reactive({
      colorNumeric(
        palette = "YlOrRd",
        domain = shape_data()$valor_promedio,
        na.color = "#808080"
      )
    })
    
    # Render the Leaflet map
    output$map <- renderLeaflet({
      # Ensure the shapefile is in the correct projection
      shape_data_transformed <- st_transform(shape_data(), 4326)
      
      leaflet() %>%
        # Base map
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        # Set view to Jalisco
        setView(lng = -103.3496, lat = 20.6597, zoom = 7) %>%
        
        # Add choropleth layer
        addPolygons(
          data = shape_data_transformed,
          fillColor = ~pal()(valor_promedio),
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
            metrica(),
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
          data = data() %>% filter(Anio == anio()),
          lng = ~lon, 
          lat = ~lat,
          radius = 4,
          color = "#1f77b4",
          stroke = FALSE,
          fillOpacity = 0.6,
          popup = ~sprintf(
            "<strong>%s</strong><br/>%s: %.2f<br/>Municipio: %s",
            Nombre_Unidad,
            metrica(),
            Dato,
            Municipio
          ) %>% lapply(HTML)
        ) %>%
        
        # Add legend
        leaflet::addLegend(
          position = "bottomright",
          pal = pal(),
          values = na.omit(shape_data()$valor_promedio),
          title = sprintf("%s <br> por Municipio", metrica()),
          opacity = 0.7,
          labFormat = labelFormat(digits = 2)
        )
    })
  })
}