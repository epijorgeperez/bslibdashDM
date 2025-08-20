# modules/indicadores_cruce.R


indicadores_cruce_UI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header("Evolución Temporal de Indicadores"),
        card_body(
          fluidRow(
            column(12,
              selectInput(ns("selected_indicator"), "Seleccionar Indicador",
                          choices = NULL, selected = NULL),
              dygraphOutput(ns("time_series_plot"), height = "400px")
            )
          )
        )
      )
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header("Análisis de Productividad vs Impacto"),
        card_body(
          fluidRow(
            column(4,
              selectInput(ns("indicator_x"), "Indicadores de Productividad (Eje X)", choices = NULL, selected = NULL)
            ),
            column(4,
              selectInput(ns("indicator_y"), "Indicadores de Impacto (Eje Y)", choices = NULL, selected = NULL)
            ),
            column(4,
              sliderInput(ns("lag_years"), "Lag Temporal (años)", 
                         min = 0, max = 5, value = 0, step = 1,
                         ticks = TRUE)
            )
          ),
          fluidRow(
            column(12,
              div(
                style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                HTML("<strong>Interpretación del Lag:</strong> Un lag de 2 años muestra la correlación entre indicadores de productividad de hace 2 años con indicadores de impacto del año actual, revelando el efecto temporal de las intervenciones.")
              )
            )
          ),
          fluidRow(
            column(12,
              plotlyOutput(ns("correlation_plot"), height = "600px")
            )
          )
        )
      )
    )
  )
}

indicadores_cruce_server <- function(id, data_indicadores, anio, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    
    # Update the choices for the indicator selections
    observe({
      all_indicators <- data_indicadores()
      
      # Filter indicators by category based on indicator codes
      productivity_indicators <- all_indicators %>%
        filter(substr(nom_indicador, 1, 5) %in% c("DM 01", "DM 02", "DM 04", "DM 05", "DM 06")) %>%
        pull(desc_indicador) %>%
        unique()
      
      impact_indicators <- all_indicators %>%
        filter(substr(nom_indicador, 1, 5) %in% c("DM 03", "DM 07", "DM 08", "DM 09")) %>%
        pull(desc_indicador) %>%
        unique()
      
      # Update dropdowns for time series (all indicators)
      all_indicator_choices <- unique(all_indicators$desc_indicador)
      updateSelectInput(session, "selected_indicator", choices = all_indicator_choices)
      
      # Update dropdowns for correlation analysis (categorized)
      updateSelectInput(session, "indicator_x", choices = productivity_indicators, 
                       selected = if(length(productivity_indicators) > 0) productivity_indicators[1] else NULL)
      updateSelectInput(session, "indicator_y", choices = impact_indicators, 
                       selected = if(length(impact_indicators) > 0) impact_indicators[1] else NULL)
    })
    
    # Update lag slider max value based on available data
    observe({
      req(anio())
      
      # Get the minimum year available in the data
      min_year <- data_indicadores() %>%
        pull(anio) %>%
        as.numeric() %>%
        min(na.rm = TRUE)
      
      # Calculate maximum possible lag
      current_year <- as.numeric(anio())
      max_lag <- current_year - min_year
      max_lag <- max(0, min(max_lag, 5))  # Cap at 5 years maximum
      
      updateSliderInput(session, "lag_years", 
                       max = max_lag,
                       value = min(input$lag_years %||% 0, max_lag))
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

    
    # Filter and prepare data for the correlation plot with lag
    correlation_data <- reactive({
        req(input$indicator_x, input$indicator_y, anio(), input$lag_years)
        
        # Calculate the years for productivity (lagged) and impact (current)
        current_year <- as.numeric(anio())
        productivity_year <- current_year - input$lag_years
        impact_year <- current_year
        
        # Get productivity data from lagged year
        productivity_data <- data_indicadores() %>%
            filter(anio == productivity_year, mes_i == 12, desc_indicador == input$indicator_x) %>%
            select(nombre_unidad, indicador_productividad = indicador)
        
        # Get impact data from current year
        impact_data <- data_indicadores() %>%
            filter(anio == impact_year, mes_i == 12, desc_indicador == input$indicator_y) %>%
            select(nombre_unidad, indicador_impacto = indicador)
        
        # Join both datasets
        combined_data <- productivity_data %>%
            inner_join(impact_data, by = "nombre_unidad") %>%
            mutate(
                indicador_productividad = as.numeric(indicador_productividad) %>% round(2),
                indicador_impacto = as.numeric(indicador_impacto) %>% round(2)
            )
        
        # Rename columns to match the original indicator names for compatibility
        names(combined_data)[names(combined_data) == "indicador_productividad"] <- input$indicator_x
        names(combined_data)[names(combined_data) == "indicador_impacto"] <- input$indicator_y
        
        return(combined_data)
    })
    
    # Get reference values for selected indicators (using respective years)
    reference_values <- reactive({
        req(input$indicator_x, input$indicator_y, anio(), input$lag_years)
        
        # Calculate the years for productivity (lagged) and impact (current)
        current_year <- as.numeric(anio())
        productivity_year <- current_year - input$lag_years
        impact_year <- current_year
        
        # Get reference values for productivity indicator from lagged year
        ref_x <- data_indicadores() %>%
            filter(anio == productivity_year, desc_indicador == input$indicator_x) %>%
            select(desc_indicador, valor_ref, lugar_esperado) %>%
            distinct() %>%
            mutate(valor_ref = as.numeric(valor_ref))
        
        # Get reference values for impact indicator from current year
        ref_y <- data_indicadores() %>%
            filter(anio == impact_year, desc_indicador == input$indicator_y) %>%
            select(desc_indicador, valor_ref, lugar_esperado) %>%
            distinct() %>%
            mutate(valor_ref = as.numeric(valor_ref))
        
        # Combine reference values
        ref_data <- bind_rows(ref_x, ref_y)
        
        # Debug: print reference values found
        message("Reference values found - Productivity year ", productivity_year, ", Impact year ", impact_year, ":")
        print(ref_data)
        
        return(ref_data)
    })

    # Create the correlation plot
    output$correlation_plot <- renderPlotly({
      req(correlation_data(), input$indicator_x, input$indicator_y, reference_values())
      
      # Check if we have enough data for the selected lag
      corr_data <- correlation_data()
      
      # Validate that we have data for the correlation
      if(nrow(corr_data) == 0) {
        productivity_year <- as.numeric(anio()) - input$lag_years
        
        # Create an empty plot with informative message
        empty_plot <- ggplot() + 
          annotate("text", x = 0, y = 0, 
                  label = paste("No hay datos suficientes para el lag seleccionado.\n",
                               "Año de productividad:", productivity_year, "\n",
                               "Año de impacto:", as.numeric(anio())), 
                  size = 6, color = "red") +
          theme_void() +
          labs(title = "Datos Insuficientes")
        
        return(ggplotly(empty_plot))
      }
      
      # Calculate statistical correlation
      x_vals <- corr_data[[input$indicator_x]]
      y_vals <- corr_data[[input$indicator_y]]
      
      # Remove NA values for correlation calculation
      complete_cases <- complete.cases(x_vals, y_vals)
      x_clean <- x_vals[complete_cases]
      y_clean <- y_vals[complete_cases]
      
      # Initialize correlation text variables
      corr_text <- ""
      significance <- ""
      
      # Calculate correlation coefficient and p-value
      if(length(x_clean) > 2 && length(y_clean) > 2) {
        corr_test <- cor.test(x_clean, y_clean, method = "pearson")
        corr_coef <- as.numeric(corr_test$estimate)
        corr_coef <- round(corr_coef, 3)
        p_value <- corr_test$p.value
        
        # Interpret correlation strength
        corr_strength <- if(abs(corr_coef) >= 0.7) {
          "Fuerte"
        } else if(abs(corr_coef) >= 0.4) {
          "Moderada"
        } else if(abs(corr_coef) >= 0.2) {
          "Débil"
        } else {
          "Muy Débil"
        }
        
        # Format p-value
        p_text <- if(p_value < 0.001) "p < 0.001" else paste("p =", round(p_value, 3))
        
        # Create correlation text for subtitle
        corr_text <- paste0("r = ", corr_coef, " (", corr_strength, "), ", p_text)
        significance <- if(p_value < 0.05) "Significativa" else "No Significativa"
        
        message("Correlation analysis: ", corr_text, " - ", significance)
      } else {
        corr_text <- "Datos insuficientes para correlación"
        significance <- ""
        message("Insufficient data for correlation analysis")
      }
      
      # Get reference values for quadrant lines
      ref_vals <- reference_values()
      x_ref <- ref_vals$valor_ref[ref_vals$desc_indicador == input$indicator_x]
      y_ref <- ref_vals$valor_ref[ref_vals$desc_indicador == input$indicator_y]
      x_lugar <- ref_vals$lugar_esperado[ref_vals$desc_indicador == input$indicator_x]
      y_lugar <- ref_vals$lugar_esperado[ref_vals$desc_indicador == input$indicator_y]
      
      # Use reference values if available, otherwise fallback to means
      x_line <- if(length(x_ref) > 0 && !is.na(x_ref)) {
        message("Using reference value for X-axis: ", x_ref)
        x_ref
      } else {
        message("No reference value found for X-axis, using mean")
        mean(correlation_data()[[input$indicator_x]], na.rm = TRUE)
      }
      
      y_line <- if(length(y_ref) > 0 && !is.na(y_ref)) {
        message("Using reference value for Y-axis: ", y_ref)
        y_ref
      } else {
        message("No reference value found for Y-axis, using mean")
        mean(correlation_data()[[input$indicator_y]], na.rm = TRUE)
      }
      
      # Determine line color and title based on whether reference values are used
      line_color <- if((length(x_ref) > 0 && !is.na(x_ref)) && (length(y_ref) > 0 && !is.na(y_ref))) "red" else "gray"
      title_suffix <- if((length(x_ref) > 0 && !is.na(x_ref)) && (length(y_ref) > 0 && !is.na(y_ref))) "(Líneas con valores de referencia)" else "(Líneas con valor promedio)"
      
      # Determine desired quadrant based on lugar_esperado
      data_range_x <- range(correlation_data()[[input$indicator_x]], na.rm = TRUE)
      data_range_y <- range(correlation_data()[[input$indicator_y]], na.rm = TRUE)
      
      desired_quadrant <- NULL
      if(length(x_lugar) > 0 && length(y_lugar) > 0 && !is.na(x_lugar) && !is.na(y_lugar)) {
        # Determine which quadrant to highlight based on lugar_esperado
        if(x_lugar == "mayor" && y_lugar == "menor") {
          # Right-Lower quadrant (Alto X, Bajo Y)
          desired_quadrant <- list(
            xmin = x_line, xmax = data_range_x[2] * 1.1,
            ymin = data_range_y[1] * 0.9, ymax = y_line
          )
          message("Desired quadrant: Right-Lower (Alto X, Bajo Y)")
        } else if(x_lugar == "menor" && y_lugar == "mayor") {
          # Left-Upper quadrant (Bajo X, Alto Y)
          desired_quadrant <- list(
            xmin = data_range_x[1] * 0.9, xmax = x_line,
            ymin = y_line, ymax = data_range_y[2] * 1.1
          )
          message("Desired quadrant: Left-Upper (Bajo X, Alto Y)")
        } else if(x_lugar == "mayor" && y_lugar == "mayor") {
          # Right-Upper quadrant (Alto X, Alto Y)
          desired_quadrant <- list(
            xmin = x_line, xmax = data_range_x[2] * 1.1,
            ymin = y_line, ymax = data_range_y[2] * 1.1
          )
          message("Desired quadrant: Right-Upper (Alto X, Alto Y)")
        } else if(x_lugar == "menor" && y_lugar == "menor") {
          # Left-Lower quadrant (Bajo X, Bajo Y)
          desired_quadrant <- list(
            xmin = data_range_x[1] * 0.9, xmax = x_line,
            ymin = data_range_y[1] * 0.9, ymax = y_line
          )
          message("Desired quadrant: Left-Lower (Bajo X, Bajo Y)")
        }
      }
      
      # Create color values vector dynamically
      color_values <- c("steelblue", "darkred", "darkgreen")
      names(color_values) <- c("Nacional", "Jalisco", unidad_medica())
      
      # Extract indicator codes (first 5 characters) for cleaner tooltips
      x_code <- substr(input$indicator_x, 1, 5)
      y_code <- substr(input$indicator_y, 1, 5)
      
      # Create the base ggplot with lag information in tooltip
      current_year <- as.numeric(anio())
      productivity_year <- current_year - input$lag_years
      impact_year <- current_year
      
      p <- ggplot(correlation_data(), 
                  aes(x = .data[[input$indicator_x]], 
                      y = .data[[input$indicator_y]],
                      text = paste0("Unidad: ", nombre_unidad,
                                  "\n", x_code, " (", productivity_year, "): ", 
                                  round(.data[[input$indicator_x]], 2),
                                  "\n", y_code, " (", impact_year, "): ", 
                                  round(.data[[input$indicator_y]], 2))))
      
      # Add desired quadrant background if available
      if(!is.null(desired_quadrant)) {
        p <- p + annotate("rect", 
                         xmin = desired_quadrant$xmin, xmax = desired_quadrant$xmax,
                         ymin = desired_quadrant$ymin, ymax = desired_quadrant$ymax,
                         fill = "lightgreen", alpha = 0.1) +
              annotate("text", 
                      x = (desired_quadrant$xmin + desired_quadrant$xmax) / 2,
                      y = (desired_quadrant$ymin + desired_quadrant$ymax) / 2,
                      label = "Cuadrante Meta", 
                      size = 4, color = "darkgreen", alpha = 0.7, fontface = "bold")
      }
      
      p <- p +
        # Add quadrant lines using reference values or means
        geom_vline(xintercept = x_line, linetype = "dashed", color = line_color, size = 1) +
        geom_hline(yintercept = y_line, linetype = "dashed", color = line_color, size = 1) +
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
        # Customize theme
        theme_minimal() +
        labs(
          title = paste("Análisis Productividad vs Impacto con Lag Temporal", 
                       if(input$lag_years > 0) paste("(Lag:", input$lag_years, "años)") else "",
                       title_suffix),
          subtitle = if(input$lag_years > 0) {
            paste("Productividad", productivity_year, "vs Impacto", impact_year, "- Diciembre")
          } else {
            paste("Año", current_year, "- Diciembre")
          },
          x = paste("Indicadores de Productividad", 
                   if(input$lag_years > 0) paste("(", productivity_year, ")") else paste("(", current_year, ")"),
                   "\n", substr(input$indicator_x, 1, 5)),
          y = paste("Indicadores de Impacto", 
                   if(input$lag_years > 0) paste("(", impact_year, ")") else paste("(", current_year, ")"),
                   "\n", substr(input$indicator_y, 1, 5)),
          color = "Unidad"
        )

      # Convert to interactive plot and add correlation annotation
      plotly_obj <- ggplotly(p, tooltip = "text")
      
      # Add correlation annotation if available
      if(corr_text != "") {
        plotly_obj <- plotly_obj %>%
          add_annotations(
            text = paste("Correlación:", corr_text, "-", significance),
            x = 0.02, y = 0.98,
            xref = "paper", yref = "paper",
            xanchor = "left", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 12, color = "darkblue"),
            bgcolor = "rgba(255,255,255,0.8)",
            bordercolor = "gray",
            borderwidth = 1
          )
      }
      
      plotly_obj
    })
  })
}