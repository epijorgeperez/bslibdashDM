# modules/indicadores_cruce.R

library(dplyr)
library(ggplot2)
library(lubridate)

indicadores_cruce_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        selectInput(ns("selected_indicator"), "Seleccionar Indicador",
                    choices = NULL, selected = NULL),
        plotOutput(ns("time_series_plot"), height = "500px")
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
        plotOutput(ns("correlation_plot"), height = "500px")
      )
    ),
    # Add this new section to display the filtered data
    fluidRow(
      column(12,
        h4("Filtered Data for Correlation Plot:"),
        verbatimTextOutput(ns("debug_data"))
      )
    )
  )
}

indicadores_cruce_server <- function(id, data_indicadores, anio, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    
    # Update the choices for the indicator selections
    observe({
      indicator_choices <- unique(data_indicadores()$nom_indicador)
      updateSelectInput(session, "selected_indicator", choices = indicator_choices)
      updateSelectInput(session, "indicator_x", choices = indicator_choices)
      updateSelectInput(session, "indicator_y", choices = indicator_choices)
    })
    
    # Filter and prepare data for the time series plot
    filtered_data <- reactive({
      req(input$selected_indicator)
      data_indicadores() %>%
        filter(nom_indicador == input$selected_indicator) %>%
        mutate(
          indicador = as.numeric(indicador),
          anio = as.numeric(anio),
          mes_i = as.numeric(mes_i),
          date = make_date(anio, mes_i, 1)
        ) %>%
        arrange(nombre_unidad, date)
    })
    
    # Create the time series plot
    output$time_series_plot <- renderPlot({
      req(filtered_data())
      
      ggplot(filtered_data(), aes(x = date, y = indicador, color = nombre_unidad, group = nombre_unidad)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(title = paste("Evolución del indicador:", input$selected_indicator),
             x = "Fecha",
             y = "Valor del Indicador",
             color = "Unidad Médica") +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, face = "bold")) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Filter and prepare data for the correlation plot
    correlation_data <- reactive({
        req(input$indicator_x, input$indicator_y, anio())
        data_indicadores() %>%
            filter(anio == anio(), mes_i == 12) %>%
            select(nombre_unidad, nom_indicador, indicador) %>%
            pivot_wider(names_from = nom_indicador, values_from = indicador) %>%
            select(nombre_unidad, !!sym(input$indicator_x), !!sym(input$indicator_y)) %>%
            mutate(across(-nombre_unidad, ~as.numeric(.) %>% round(2)))
        })

    
    # Output the selected anio, indicator x, and y, and top 5 rows of correlation_data for debugging
    output$debug_data <- renderPrint({
        req(anio(), input$indicator_x, input$indicator_y, correlation_data())
        print(paste("Año:", anio(), "- Diciembre"))
        print(paste("Indicador X:", input$indicator_x, "Indicador Y:", input$indicator_y))
        print("Filtered and transformed data:")
        print(correlation_data())
    })
    
    # Create the correlation plot
    output$correlation_plot <- renderPlot({
  req(correlation_data())
  
  if(nrow(correlation_data()) == 0 || 
     is.null(correlation_data()[[input$indicator_x]]) || 
     is.null(correlation_data()[[input$indicator_y]])) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected indicators") +
             theme_void())
  }
  
  ggplot(correlation_data(), aes(x = !!sym(input$indicator_x), y = !!sym(input$indicator_y))) +
    geom_point(aes(color = nombre_unidad), size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("Correlación entre", input$indicator_x, "y", input$indicator_y),
         subtitle = paste("Año:", anio(), "- Diciembre"),
         x = input$indicator_x,
         y = input$indicator_y,
         color = "Unidad Médica") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
})
  })
}