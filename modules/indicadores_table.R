# modules/indicadores_table.R


# Data preparation function (unchanged)
prepare_indicadores_data <- function(data, selected_year, selected_type, selected_indicator) {
  tryCatch({
    filtered_data <- data %>%
      filter(anio == selected_year, tipo == selected_type, nom_indicador == selected_indicator)
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    filtered_data %>%
      mutate(
        Valor_Indicador = as.numeric(indicador),
        Mes = factor(mes_i, levels = 1:12, labels = month.abb)
      ) %>%
      select(
        Mes,
        Indicador = nom_indicador,
        Numerador = numerador,
        Denominador = denominador,
        Valor_Indicador
      ) %>%
      arrange(Mes)
  }, error = function(e) {
    warning(paste("Error in prepare_indicadores_data:", e$message))
    return(NULL)
  })
}

indicadores_table_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("indicator_type"), "Tipo de Indicador", 
                choices = c("OOAD", "Unidad")),
    selectInput(ns("indicator"), "Indicador", 
                choices = c("DM 01", "DM 02", "DM 03")),
    gt_output(ns("indicadores_table"))
  )
}

# Updated server function to include unidad_medica parameter
indicadores_table_server <- function(id, data_indicadores, anio, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    
    output$indicadores_table <- render_gt({
      req(input$indicator_type, input$indicator, data_indicadores(), anio(), unidad_medica())
      
      # Prepare data based on user selections
      prepared_data <- prepare_indicadores_data(
        data_indicadores(),
        selected_year = anio(),
        selected_type = input$indicator_type,
        selected_indicator = input$indicator
      )
      
      # Check if prepared_data is NULL or has 0 rows
      if (is.null(prepared_data) || nrow(prepared_data) == 0) {
        return(gt() %>% 
          tab_header(title = "No data available") %>%
          tab_source_note(
            source_note = "Please check your selections and try again."
          )
        )
      }
      
      # Create the table using gt with enhanced styling
      prepared_data %>%
        gt() %>%
        tab_header(
          title = md(paste("**Indicador de Diabetes:**", input$indicator)),
          subtitle = md(paste0("**Año:** ", anio(), " | **Unidad Médica:** ", unidad_medica(), " | **Tipo:** ", input$indicator_type))
        ) %>%
        fmt_number(
          columns = c(Numerador, Denominador),
          decimals = 0,
          use_seps = TRUE
        ) %>%
        fmt_number(
          columns = Valor_Indicador,
          decimals = 2
        ) %>%
        cols_label(
          Mes = md("**Mes**"),
          Numerador = md("**Numerador**"),
          Denominador = md("**Denominador**"),
          Valor_Indicador = md("**Valor del Indicador**")
        ) %>%
        tab_style(
          style = list(
            cell_borders(
              sides = "bottom",
              color = "#D3D3D3",
              weight = px(1)
            )
          ),
          locations = cells_body()
        ) %>%
        tab_style(
          style = list(
            cell_fill(color = "#F0F0F0"),
            cell_text(weight = "bold")
          ),
          locations = cells_column_labels()
        ) %>%
        tab_options(
          table.width = px(800),
          table.border.top.style = "hidden",
          table.border.bottom.style = "hidden",
          column_labels.border.top.style = "hidden",
          column_labels.border.bottom.color = "#666666",
          column_labels.border.bottom.width = 2,
          table_body.border.bottom.color = "#666666",
          table_body.border.bottom.width = 2,
          data_row.padding = px(10),
          source_notes.font.size = 12,
          table.font.size = 14,
          heading.title.font.size = 18,
          heading.subtitle.font.size = 14
        ) %>%
        tab_style(
          style = cell_text(color = "#666666"),
          locations = cells_body()
        ) %>%
        tab_source_note(
          source_note = md("*Fuente: Sistema de Información de Diabetes*")
        ) %>%
        opt_css(
          css = "
            .gt_table {
              font-family: Arial, sans-serif;
            }
            .gt_heading {
              background-color: #FFFFFF;
              border-bottom: 2px solid #666666;
              padding-bottom: 10px;
            }
            .gt_title {
              color: #333333;
            }
            .gt_subtitle {
              color: #666666;
            }
          "
        )
    })
  })
}