# modules/time_chart.R

time_chart_UI <- function(id) {
  ns <- NS(id)
  dygraphOutput(ns("time_chart"), height = "400px", width = "100%")
}

library(htmlwidgets)

time_chart_server <- function(id, metrica, totales_anuales, totales_consultas, totales_incap, totales_hosp, totales_mortalidad, totales_incidencia, ooad, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    output$time_chart <- renderDygraph({
      # Validate that required inputs are available
      req(metrica(), unidad_medica())
      
      # Get the corresponding data based on the selected metric
      data <- switch(metrica(),
                     "Prevalencia" = totales_anuales(),
                     "Consultas" = totales_consultas(),
                     "Incapacidades" = totales_incap(),
                     "Hospitalizaciones" = totales_hosp(),
                     "Mortalidad" = totales_mortalidad(),
                     "Incidencia" = totales_incidencia())
      
      req(data) # Ensure data is available

      # Filter the data based on the user's input
      filtered <- data %>%
        filter(Nombre_Unidad == unidad_medica()) %>%
        mutate(Anio = ymd(paste(Anio, "-01-01", sep = ""))) %>%
        select(Anio, Dato)  # Select only the necessary columns

      # Calculate the range of the data for y-axis limits
      y_range <- range(filtered$Dato, na.rm = TRUE)
      y_max_with_padding <- y_range[2] * 1.35  # Add 35% padding above maximum value

      # Custom JavaScript function for formatting dates
      formatDate <- JS("function(d) {
        return d.getFullYear();
      }")

      # Creating the dygraph
      dygraph(filtered, xlab = "Año", ylab = metrica()) %>%
        dySeries('Dato', label = metrica(), strokeWidth = 2, color = "steelblue") %>%
        dyOptions(
          stackedGraph = FALSE,  # Changed to FALSE for better visibility of trends
          fillGraph = TRUE,
          fillAlpha = 0.4,
          drawGrid = FALSE,
          colors = ("steelblue")
        ) %>%
        dyAxis("y", label = metrica(), valueRange = c(0, y_max_with_padding)) %>%
        dyAxis("x", label = "Año", axisLabelFormatter = formatDate) %>%
        #dyRangeSelector() %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
        #dyRoller(rollPeriod = 1) %>%
        dyLegend(show = "always", hideOnMouseOut = FALSE)
    })
  })
}