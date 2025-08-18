# modules/simple_forecasting.R

simple_forecasting_UI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 9),
      card(
        card_header("Configuración de Pronóstico"),
        card_body(
          selectInput(ns("forecast_method"), "Método de Pronóstico:",
                     choices = c("Automático (ETS/ARIMA)" = "auto",
                               "ARIMA" = "arima", 
                               "ETS (Suavizado)" = "ets",
                               "Naive" = "naive"),
                     selected = "auto"),
          hr(),
          h5("📊 Granularidad Temporal"),
          radioButtons(ns("granularity"), NULL,
                      choices = c("📅 Anual (Tendencias a largo plazo)" = "yearly",
                                "📈 Mensual (Patrones estacionales)" = "monthly"),
                      selected = "yearly",
                      inline = FALSE),
          uiOutput(ns("forecast_horizon_ui")),
          selectInput(ns("confidence_level"), "Nivel de Confianza:",
                     choices = c("80%" = 80, "90%" = 90, "95%" = 95),
                     selected = 95),
          hr(),
          h5("Información del Modelo"),
          verbatimTextOutput(ns("model_info"), placeholder = TRUE)
        )
      ),
      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("forecast_title"))),
        card_body(
          dygraphOutput(ns("forecast_plot"), height = "500px")
        )
      )
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(6, 6),
      card(
        card_header("Métricas de Precisión del Modelo"),
        card_body(
          DT::dataTableOutput(ns("accuracy_table"))
        )
      ),
      card(
        card_header("Valores Pronosticados"),
        card_body(
          DT::dataTableOutput(ns("forecast_table"))
        )
      )
    )
  )
}

simple_forecasting_server <- function(id, metrica, totales_anuales, totales_consultas, totales_incap, totales_hosp, totales_mortalidad, totales_incidencia, totales_anuales_mensuales, totales_consultas_mensuales, totales_incap_mensuales, totales_hosp_mensuales, totales_mortalidad_mensuales, totales_incidencia_mensuales, ooad, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    
    # Dynamic title for the forecast card showing metric and unit
    output$forecast_title <- renderUI({
      req(metrica(), unidad_medica())
      h4(paste("Pronóstico de", metrica(), "- Unidad:", unidad_medica()), 
         style = "margin: 0; color: #2c3e50;")
    })
    
    # Dynamic forecast horizon UI based on granularity
    output$forecast_horizon_ui <- renderUI({
      req(input$granularity)
      
      if(input$granularity == "monthly") {
        tagList(
          numericInput(session$ns("forecast_horizon"), 
                      "Meses a Pronosticar:",
                      value = 24, min = 1, max = 36, step = 1),
          helpText("Pronósticos de 6-24 meses son más confiables")
        )
      } else {
        tagList(
          numericInput(session$ns("forecast_horizon"), 
                      "Años a Pronosticar:",
                      value = 3, min = 1, max = 10, step = 1),
          helpText("Pronósticos de 2-6 años son más confiables")
        )
      }
    })
    
    # Reactive data preparation
    forecast_data <- reactive({
      req(metrica(), unidad_medica(), input$granularity)
      
      # Get the corresponding data based on the selected metric and granularity
      if(input$granularity == "monthly") {
        data <- switch(metrica(),
                       "Prevalencia" = totales_anuales_mensuales(),
                       "Consultas" = totales_consultas_mensuales(),
                       "Incapacidades" = totales_incap_mensuales(),
                       "Hospitalizaciones" = totales_hosp_mensuales(),
                       "Mortalidad" = totales_mortalidad_mensuales(),
                       "Incidencia" = totales_incidencia_mensuales())
      } else {
        data <- switch(metrica(),
                       "Prevalencia" = totales_anuales(),
                       "Consultas" = totales_consultas(),
                       "Incapacidades" = totales_incap(),
                       "Hospitalizaciones" = totales_hosp(),
                       "Mortalidad" = totales_mortalidad(),
                       "Incidencia" = totales_incidencia())
      }
      
      req(data)
      
      # Handle different data structures for monthly vs yearly
      if(input$granularity == "monthly") {
        # Monthly data processing
        current_date <- Sys.Date()
        current_year <- as.numeric(format(current_date, "%Y"))
        current_month <- as.numeric(format(current_date, "%m"))
        
        # Only use complete months (exclude current month if incomplete)
        last_complete_date <- if(current_month == 1) {
          as.Date(paste(current_year - 1, "12", "01", sep = "-"))
        } else {
          as.Date(paste(current_year, sprintf("%02d", current_month - 1), "01", sep = "-"))
        }
        
        filtered <- data %>%
          filter(Nombre_Unidad == unidad_medica()) %>%
          filter(fecha <= last_complete_date) %>%
          arrange(fecha) %>%
          select(fecha, Anio, Mes, Dato)
        
        # Ensure we have enough data points for monthly forecasting (minimum 24 months)
        if(nrow(filtered) < 24) {
          message("Insufficient monthly data for forecasting: only ", nrow(filtered), " months available (minimum 24 required)")
          return(NULL)
        }
        
        message("Using monthly data from ", min(filtered$fecha), " to ", max(filtered$fecha), 
                " (", nrow(filtered), " months) for forecasting")
        
        # Create monthly time series object
        start_year <- as.numeric(min(filtered$Anio))
        start_month <- as.numeric(min(filtered[filtered$Anio == start_year, "Mes"]))
        ts_data <- ts(as.numeric(filtered$Dato), start = c(start_year, start_month), frequency = 12)
        
        return(list(
          ts_obj = ts_data,
          dates = filtered$fecha,
          years = filtered$Anio,
          months = filtered$Mes,
          values = filtered$Dato,
          granularity = "monthly"
        ))
        
      } else {
        # Yearly data processing (original logic)
        current_year <- as.numeric(format(Sys.Date(), "%Y"))
        last_complete_year <- current_year - 1
        
        filtered <- data %>%
          filter(Nombre_Unidad == unidad_medica()) %>%
          mutate(Anio = as.numeric(as.character(Anio))) %>%
          filter(Anio <= last_complete_year) %>%
          arrange(Anio) %>%
          select(Anio, Dato)
        
        # Ensure we have enough data points for yearly forecasting
        if(nrow(filtered) < 3) {
          message("Insufficient yearly data for forecasting: only ", nrow(filtered), " complete years available")
          return(NULL)
        }
        
        message("Using yearly data from ", min(filtered$Anio), " to ", max(filtered$Anio), 
                " (", nrow(filtered), " complete years) for forecasting")
        
        # Create yearly time series object
        ts_data <- ts(as.numeric(filtered$Dato), start = min(filtered$Anio, na.rm = TRUE), frequency = 1)
        
        return(list(
          ts_obj = ts_data,
          years = filtered$Anio,
          values = filtered$Dato,
          granularity = "yearly"
        ))
      }
    })
    
    # Reactive forecast model
    forecast_model <- reactive({
      req(forecast_data(), input$forecast_method, input$forecast_horizon)
      
      ts_data <- forecast_data()$ts_obj
      
      # Apply selected forecasting method
      model <- tryCatch({
        switch(input$forecast_method,
          "auto" = {
            # Compare ETS and ARIMA models based on AIC
            models <- list()
            
            # Try ETS
            models$ets <- tryCatch(ets(ts_data), error = function(e) NULL)
            
            # Try ARIMA
            models$arima <- tryCatch(auto.arima(ts_data, stepwise = TRUE, approximation = TRUE), error = function(e) NULL)
            
            # Remove NULL models
            valid_models <- models[!sapply(models, is.null)]
            
            if(length(valid_models) == 0) {
              # Both failed, use RWF as fallback
              message("Auto mode: Both ETS and ARIMA failed, using RWF as fallback")
              rwf(ts_data)
            } else if(length(valid_models) == 1) {
              # Only one model worked
              model_name <- names(valid_models)[1]
              message("Auto mode selected: ", model_name, " (only viable option)")
              valid_models[[1]]
            } else {
              # Both models worked, compare AICs
              aics <- sapply(valid_models, function(m) m$aic)
              best_model_name <- names(which.min(aics))
              message("Auto mode selected: ", best_model_name, " (AIC: ", round(min(aics), 2), 
                     " vs ", round(max(aics), 2), ")")
              valid_models[[best_model_name]]
            }
          },
          "arima" = auto.arima(ts_data, stepwise = TRUE, approximation = TRUE),
          "ets" = ets(ts_data),
          "naive" = {
            # Simple naive method for baseline comparison
            rwf(ts_data)  # rwf is the preferred function for naive forecasts
          }
        )
      }, error = function(e) {
        message("Forecasting error: ", e$message)
        # Fallback to naive method
        rwf(ts_data)
      })
      
      return(model)
    })
    
    # Reactive forecast
    forecast_result <- reactive({
      req(forecast_model(), input$forecast_horizon, input$confidence_level)
      
      model <- forecast_model()
      h <- as.numeric(input$forecast_horizon)
      level <- as.numeric(input$confidence_level)
      
      # Generate forecast with explicit level parameter
      fc <- tryCatch({
        if(inherits(model, "rwf")) {
          # For RWF models, we need to regenerate with the level parameter
          ts_data <- forecast_data()$ts_obj
          rwf(ts_data, h = h, level = level)
        } else {
          # For ETS and ARIMA models
          forecast(model, h = h, level = level)
        }
      }, error = function(e) {
        message("Forecast generation error: ", e$message)
        # Emergency fallback
        ts_data <- forecast_data()$ts_obj
        rwf(ts_data, h = h, level = level)
      })
      
      return(fc)
    })
    
    # Model information output
    output$model_info <- renderText({
      req(forecast_model())
      
      model <- forecast_model()
      
      if(inherits(model, "ets")) {
        paste0("Método: ETS(", model$method, ")\n",
               "AIC: ", round(model$aic, 2), "\n",
               "Parámetros: α=", round(model$par[1], 3),
               if(length(model$par) > 1) paste0(", β=", round(model$par[2], 3)),
               if(length(model$par) > 2) paste0(", γ=", round(model$par[3], 3)))
      } else if(inherits(model, "Arima")) {
        paste0("Método: ARIMA(", paste(model$arma[c(1,6,2)], collapse=","), ")\n",
               "AIC: ", round(model$aic, 2), "\n",
               "Sigma²: ", round(model$sigma2, 4))
      } else if(inherits(model, "rwf") || inherits(model, "naive")) {
        "Método: Naive (Random Walk)\nModelo base de comparación"
      } else {
        paste("Método:", class(model)[1])
      }
    })
    
    # Forecast plot
    output$forecast_plot <- renderDygraph({
      req(forecast_result(), forecast_data(), input$granularity)
      
      fc <- forecast_result()
      data_info <- forecast_data()
      
      # Handle different time structures for monthly vs yearly
      if(data_info$granularity == "monthly") {
        # Monthly data processing
        hist_dates <- data_info$dates
        hist_values <- data_info$values
        
        # Generate forecast dates
        last_date <- max(hist_dates, na.rm = TRUE)
        forecast_dates <- seq(from = last_date + months(1), 
                             by = "month", 
                             length.out = length(fc$mean))
        
        # Combine historical and forecast data
        all_dates <- c(hist_dates, forecast_dates)
        
        # Create data frame for dygraph
        n_hist <- length(hist_values) 
        n_forecast <- length(fc$mean)
        
        # Initialize vectors
        historical <- c(hist_values, rep(NA, n_forecast))
        forecast_mean <- c(rep(NA, n_hist), as.numeric(fc$mean))
        
        # Extract confidence intervals
        if(!is.null(fc$upper) && !is.null(fc$lower)) {
          forecast_upper <- c(rep(NA, n_hist), as.numeric(fc$upper[,1]))
          forecast_lower <- c(rep(NA, n_hist), as.numeric(fc$lower[,1]))
        } else {
          std_err <- sd(hist_values, na.rm = TRUE) * sqrt(1:n_forecast)
          forecast_upper <- c(rep(NA, n_hist), as.numeric(fc$mean) + 1.96 * std_err)
          forecast_lower <- c(rep(NA, n_hist), as.numeric(fc$mean) - 1.96 * std_err)
        }
        
        # Create data frame with dates
        df <- data.frame(
          Date = all_dates,
          Historical = historical,
          Forecast = forecast_mean,
          Upper = forecast_upper,
          Lower = forecast_lower
        )
        
        # Create dygraph with monthly formatting
        dygraph(df, xlab = "Fecha", ylab = metrica()) %>%
          dySeries("Historical", label = "Histórico", color = "steelblue", strokeWidth = 2) %>%
          dySeries("Forecast", label = "Pronóstico", color = "red", strokeWidth = 2, strokePattern = "dashed") %>%
          dySeries(c("Lower", "Forecast", "Upper"), label = paste0("IC ", input$confidence_level, "%"), color = "lightcoral") %>%
          dyOptions(
            fillGraph = FALSE,
            drawGrid = TRUE,
            gridLineColor = "lightgray"
          ) %>%
          dyAxis("y", label = metrica()) %>%
          dyAxis("x", label = "Fecha", drawGrid = TRUE) %>%
          dyHighlight(highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.3) %>%
          dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
          dyShading(from = min(forecast_dates), to = max(forecast_dates), color = "#FFE6E6")
        
      } else {
        # Yearly data processing (original logic)
        hist_years <- data_info$years
        hist_values <- data_info$values
        
        # Forecast data
        last_year <- as.numeric(max(hist_years, na.rm = TRUE))
        forecast_years <- seq(last_year + 1, last_year + length(fc$mean))
        
        # Combine historical and forecast data
        all_years <- c(as.numeric(hist_years), forecast_years)
        
        # Create data frame for dygraph
        n_hist <- length(hist_values) 
        n_forecast <- length(fc$mean)
        
        # Initialize vectors
        historical <- c(hist_values, rep(NA, n_forecast))
        forecast_mean <- c(rep(NA, n_hist), as.numeric(fc$mean))
        
        # Extract confidence intervals
        if(!is.null(fc$upper) && !is.null(fc$lower)) {
          forecast_upper <- c(rep(NA, n_hist), as.numeric(fc$upper[,1]))
          forecast_lower <- c(rep(NA, n_hist), as.numeric(fc$lower[,1]))
        } else {
          std_err <- sd(hist_values, na.rm = TRUE) * sqrt(1:n_forecast)
          forecast_upper <- c(rep(NA, n_hist), as.numeric(fc$mean) + 1.96 * std_err)
          forecast_lower <- c(rep(NA, n_hist), as.numeric(fc$mean) - 1.96 * std_err)
        }
        
        # Create data frame
        df <- data.frame(
          Year = all_years,
          Historical = historical,
          Forecast = forecast_mean,
          Upper = forecast_upper,
          Lower = forecast_lower
        )
        
        # Create dygraph with yearly formatting
        dygraph(df, xlab = "Año", ylab = metrica()) %>%
          dySeries("Historical", label = "Histórico", color = "steelblue", strokeWidth = 2) %>%
          dySeries("Forecast", label = "Pronóstico", color = "red", strokeWidth = 2, strokePattern = "dashed") %>%
          dySeries(c("Lower", "Forecast", "Upper"), label = paste0("IC ", input$confidence_level, "%"), color = "lightcoral") %>%
          dyOptions(
            fillGraph = FALSE,
            drawGrid = TRUE,
            gridLineColor = "lightgray"
          ) %>%
          dyAxis("y", label = metrica()) %>%
          dyAxis("x", label = "Año") %>%
          dyHighlight(highlightCircleSize = 4, highlightSeriesBackgroundAlpha = 0.3) %>%
          dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
          dyShading(from = min(forecast_years), to = max(forecast_years), color = "#FFE6E6")
      }
    })
    
    # Accuracy metrics table
    output$accuracy_table <- DT::renderDataTable({
      req(forecast_model(), forecast_data())
      
      model <- forecast_model()
      
      # Calculate accuracy metrics if possible
      accuracy_df <- tryCatch({
        acc <- accuracy(model)
        
        # Create a clean data frame
        metrics_df <- data.frame(
          Métrica = c("RMSE", "MAE", "MAPE", "MASE"),
          Valor = c(
            round(acc[,"RMSE"], 3),
            round(acc[,"MAE"], 3), 
            round(acc[,"MAPE"], 2),
            if("MASE" %in% colnames(acc)) round(acc[,"MASE"], 3) else NA
          ),
          Descripción = c(
            "Error cuadrático medio",
            "Error absoluto medio", 
            "Error porcentual absoluto medio",
            "Error escalado absoluto medio"
          )
        )
        
        # Remove NA rows
        metrics_df[!is.na(metrics_df$Valor), ]
        
      }, error = function(e) {
        data.frame(
          Métrica = "Error",
          Valor = "N/A",
          Descripción = "No se pudieron calcular métricas"
        )
      })
      
      DT::datatable(accuracy_df, 
                    options = list(dom = 't', pageLength = 10),
                    rownames = FALSE) %>%
        DT::formatRound(columns = "Valor", digits = 3)
    })
    
    # Forecast values table
    output$forecast_table <- DT::renderDataTable({
      req(forecast_result(), forecast_data(), input$granularity)
      
      fc <- forecast_result()
      data_info <- forecast_data()
      
      # Handle different time structures for monthly vs yearly
      if(data_info$granularity == "monthly") {
        # Monthly forecast table
        last_date <- max(data_info$dates, na.rm = TRUE)
        forecast_dates <- seq(from = last_date + months(1), 
                             by = "month", 
                             length.out = length(fc$mean))
        
        forecast_df <- data.frame(
          Fecha = format(forecast_dates, "%Y-%m"),
          Período = paste(format(forecast_dates, "%B %Y")),
          Pronóstico = round(as.numeric(fc$mean), 2),
          `Límite Inferior` = if(!is.null(fc$lower)) round(as.numeric(fc$lower[,1]), 2) else NA,
          `Límite Superior` = if(!is.null(fc$upper)) round(as.numeric(fc$upper[,1]), 2) else NA
        )
        
        # Remove NA columns
        forecast_df <- forecast_df[, !sapply(forecast_df, function(x) all(is.na(x)))]
        
        DT::datatable(forecast_df,
                      options = list(dom = 't', pageLength = 20, scrollY = "400px"),
                      rownames = FALSE) %>%
          DT::formatRound(columns = c("Pronóstico", "Límite.Inferior", "Límite.Superior"), digits = 2)
        
      } else {
        # Yearly forecast table (original logic)
        last_year <- as.numeric(max(data_info$years, na.rm = TRUE))
        forecast_years <- seq(last_year + 1, last_year + length(fc$mean))
        
        forecast_df <- data.frame(
          Año = forecast_years,
          Pronóstico = round(as.numeric(fc$mean), 2),
          `Límite Inferior` = if(!is.null(fc$lower)) round(as.numeric(fc$lower[,1]), 2) else NA,
          `Límite Superior` = if(!is.null(fc$upper)) round(as.numeric(fc$upper[,1]), 2) else NA
        )
        
        # Remove NA columns
        forecast_df <- forecast_df[, !sapply(forecast_df, function(x) all(is.na(x)))]
        
        DT::datatable(forecast_df,
                      options = list(dom = 't', pageLength = 15),
                      rownames = FALSE) %>%
          DT::formatRound(columns = c("Pronóstico", "Límite.Inferior", "Límite.Superior"), digits = 2)
      }
    })
  })
}