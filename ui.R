# ui.R

source("modules/metric_summary.R")
source("modules/age_sex_graph.R")
source("modules/time_chart.R")
source("modules/map_chart.R")
source("modules/test_mod.R")
source("modules/indicadores_table.R")

ui <- page_navbar(
  title = tagList(
    "Diabetes CIIMSS Prototipo",
    tags$img(
      src = "gobimss.png",
      height = "35px",
      style = "float: left; margin-right: 15px;"
    )
  ),
  id = "navbar",
  sidebar = sidebar(
    selectInput("anio", "Año:", choices = NULL, selectize = TRUE),
    selectInput("ooad", "OOAD:", choices = NULL),
    selectInput("unidad_medica", "Unidad Médica:", choices = NULL),
    selectInput("metric_choice", "Métrica a analizar:", choices = metric_choices, selected = "Prevalencia")
  ),
  nav_panel(
    title = "Métricas principales",
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      metric_summary_UI("metric_summary"),
      height = "220px"
    ),
    layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        card(
          full_screen = TRUE,
          card_header("Curva Temporal"),
          time_chart_UI("time_chart")
        ),
        card(
          full_screen = TRUE,
          card_header("Gráfico Edad y Sexo"),
          age_sex_graph_UI("age_sex_graph")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Mapa"),
        map_chart_ui("map")
      )
    )
  ),
  nav_panel(
    title = "Indicadores Médicos",
    layout_column_wrap(
    #  card(
    #   width = 1/3,
    #    full_screen = TRUE,
    #    card_header("Módulo de Pruebas"),
    #    test_mod_UI("")
    #  ),
      card(
        width = 1,
        full_screen = TRUE,
        card_header("Indicadores"),
        indicadores_table_UI("indicadores_table")
      )
    )
  )
)