# modules/indicadores_cruce.R


indicadores_cruce_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        selectInput(ns("selected_indicator"), "Seleccionar Indicador",
                    choices = NULL, selected = NULL),
        dygraphOutput(ns("time_series_plot"), height = "500px")
      )
    ),
    fluidRow(
      column(6,
        selectInput(ns("indicator_x"), "Indicador X", choices = NULL, selected = NULL)
      ),
      column(6,
        selectInput(ns("indicator_y"), "Indicador Y", choices = NULL, selected = NULL)
      )
    ),
    fluidRow(
      column(12,
        plotlyOutput(ns("correlation_plot"), height = "700px")
      )
    )
  )
}

indicadores_cruce_server <- function(id, data_indicadores, anio, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    
    # Update the choices for the indicator selections
    observe({
      indicator_choices <- unique(data_indicadores()$desc_indicador)
      updateSelectInput(session, "selected_indicator", choices = indicator_choices)
      updateSelectInput(session, "indicator_x", choices = indicator_choices)
      updateSelectInput(session, "indicator_y", choices = indicator_choices,selected = indicator_choices[2])
    })
    
    # Modified filtered_data to prepare for dygraph
    filtered_data <- reactive({
      req(input$selected_indicator)
      data_indicadores() %>%
        filter(desc_indicador == input$selected_indicator,
               nombre_unidad %in% c("Nacional", "Jalisco", unidad_medica())) %>%
        mutate(
          indicador = as.numeric(indicador),
          anio = as.numeric(anio),
          mes_i = as.numeric(mes_i),
          date = make_date(anio, mes_i, 1)
        ) %>%
        arrange(date) %>%
        select(date, nombre_unidad, indicador) %>%
        pivot_wider(names_from = nombre_unidad, 
                   values_from = indicador)
    })
    
    # Replace the ggplot with dygraph
    output$time_series_plot <- renderDygraph({
      req(filtered_data())
      
      # Custom date formatter
      formatDate <- JS("function(d) {
        var months = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 
                     'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'];
        return months[d.getMonth()] + ' ' + d.getFullYear();
      }")

      # Calculate y-axis range for padding
      all_values <- as.matrix(filtered_data()[,-1])
      y_range <- range(all_values, na.rm = TRUE)
      y_padding <- diff(y_range) * 0.1

      dygraph(filtered_data(), xlab = "Fecha", ylab = input$selected_indicator) %>%
        dyOptions(
          fillGraph = TRUE,
          fillAlpha = 0.1,
          drawGrid = FALSE,
          colors = c("steelblue", "darkred", "darkgreen")
        ) %>%
        dyAxis("y", 
               label = input$selected_indicator,
               valueRange = c(y_range[1] - y_padding, y_range[2] + y_padding)) %>%
        dyAxis("x", 
               label = "Fecha",
               axisLabelFormatter = formatDate) %>%
        dyHighlight(
          highlightCircleSize = 5,
          highlightSeriesBackgroundAlpha = 0.5,
          hideOnMouseOut = FALSE
        ) %>%
        dyLegend(show = "follow", 
                 hideOnMouseOut = FALSE, labelsSeparateLines = TRUE)
    })

    
    # Filter and prepare data for the correlation plot
    correlation_data <- reactive({
        req(input$indicator_x, input$indicator_y, anio())
        data_indicadores() %>%
            filter(anio == anio(), mes_i == 12) %>%
            select(nombre_unidad, desc_indicador, indicador) %>%
            pivot_wider(
              names_from = desc_indicador, 
              values_from = indicador,
              names_repair = "unique_quiet"
            ) %>%
            select(nombre_unidad, all_of(c(input$indicator_x, input$indicator_y))) %>%
            mutate(across(-nombre_unidad, ~as.numeric(.) %>% round(2)))
    })

    # Create the correlation plot
    output$correlation_plot <- renderPlotly({
      req(correlation_data(), input$indicator_x, input$indicator_y)
      
      # Calculate means for quadrant lines
      x_mean <- mean(correlation_data()[[input$indicator_x]], na.rm = TRUE)
      y_mean <- mean(correlation_data()[[input$indicator_y]], na.rm = TRUE)
      
      # Create color values vector dynamically
      color_values <- c("steelblue", "darkred", "darkgreen")
      names(color_values) <- c("Nacional", "Jalisco", unidad_medica())
      
      # Create the base ggplot
      p <- ggplot(correlation_data(), 
                  aes(x = .data[[input$indicator_x]], 
                      y = .data[[input$indicator_y]],
                      text = paste0("Unidad: ", nombre_unidad,
                                  "\n", input$indicator_x, ": ", 
                                  round(.data[[input$indicator_x]], 2),
                                  "\n", input$indicator_y, ": ", 
                                  round(.data[[input$indicator_y]], 2)))) +
        # Add quadrant lines
        geom_vline(xintercept = x_mean, linetype = "dashed", color = "gray") +
        geom_hline(yintercept = y_mean, linetype = "dashed", color = "gray") +
        # Add regular points
        geom_point(data = . %>% filter(!nombre_unidad %in% c(unidad_medica(), "Nacional", "Jalisco")),
                  size = 1.5, alpha = 0.6, color = "gray70") +
        # Add highlighted points
        geom_point(data = . %>% filter(nombre_unidad %in% c(unidad_medica(), "Nacional", "Jalisco")),
                  aes(color = nombre_unidad), size = 3) +
        # Add labels for highlighted points
        geom_text_repel(data = . %>% filter(nombre_unidad %in% c(unidad_medica(), "Nacional", "Jalisco")),
                       aes(label = nombre_unidad, color = nombre_unidad), 
                       size = 4, show.legend = FALSE) +
        # Custom colors
        scale_color_manual(values = color_values) +
        # Add quadrant labels
        annotate("text", x = max(correlation_data()[[input$indicator_x]], na.rm = TRUE), 
                y = max(correlation_data()[[input$indicator_y]], na.rm = TRUE), 
                label = "Alto-Alto", hjust = 1, vjust = 1) +
        annotate("text", x = min(correlation_data()[[input$indicator_x]], na.rm = TRUE), 
                y = max(correlation_data()[[input$indicator_y]], na.rm = TRUE), 
                label = "Bajo-Alto", hjust = 0, vjust = 1) +
        annotate("text", x = max(correlation_data()[[input$indicator_x]], na.rm = TRUE), 
                y = min(correlation_data()[[input$indicator_y]], na.rm = TRUE), 
                label = "Alto-Bajo", hjust = 1, vjust = 0) +
        annotate("text", x = min(correlation_data()[[input$indicator_x]], na.rm = TRUE), 
                y = min(correlation_data()[[input$indicator_y]], na.rm = TRUE), 
                label = "Bajo-Bajo", hjust = 0, vjust = 0) +
        # Customize theme
        theme_minimal() +
        labs(
          title = paste("Análisis de cuadrante:", anio(), "- Diciembre"),
          x = input$indicator_x,
          y = input$indicator_y,
          color = "Unidad"
        )

      # Convert to interactive plot
      ggplotly(p, tooltip = "text")
    })
  })
}