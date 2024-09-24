# modules/time_chart.R

time_chart_UI <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_body(
      dygraphOutput(ns("time_chart"), height = "300px", width = "100%")
    )
  )
}

library(htmlwidgets)

time_chart_server <- function(id, metrica, totales_anuales, totales_consultas, totales_incap, totales_hosp, totales_mortalidad, totales_incidencia, ooad, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    output$time_chart <- renderDygraph({
      # Get the corresponding data based on the selected metric
      data <- switch(metrica(),
                     "Prevalencia" = totales_anuales(),
                     "Consultas" = totales_consultas(),
                     "Incapacidades" = totales_incap(),
                     "Hospitalizaciones" = totales_hosp(),
                     "Mortalidad" = totales_mortalidad(),
                     "Incidencia" = totales_incidencia())

      # Filter the data based on the user's input
      filtered <- data %>%
        filter(Nombre_Unidad == unidad_medica()) %>%
        mutate(Anio = ymd(paste(Anio, "-01-01", sep = ""))) %>%
        select(Anio, Dato)  # Select only the necessary columns

      # Calculate the range of the data for y-axis limits
      y_range <- range(filtered$Dato, na.rm = TRUE)
      y_padding <- diff(y_range) * 0.1  # Add 10% padding

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
        dyAxis("y", label = metrica(), valueRange = c(y_range[1] - y_padding, y_range[2] + y_padding)) %>%
        dyAxis("x", label = "Año", axisLabelFormatter = formatDate) %>%
        #dyRangeSelector() %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
        #dyRoller(rollPeriod = 1) %>%
        dyLegend(show = "always", hideOnMouseOut = FALSE)
    })
  })
}