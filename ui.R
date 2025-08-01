# ui.R

source("modules/inicio_portada.R")
source("modules/metric_summary.R")
source("modules/age_sex_graph.R")
source("modules/time_chart.R")
source("modules/map_chart.R")
source("modules/test_mod.R")
#source("modules/indicadores_table.R")
source("modules/indicadores_cruce.R")

ui <- page_navbar(
  title = tagList(
    "Diabetes CIIMSS",
    tags$img(
      src = "gobimss.png",
      height = "35px",
      style = "float: left; margin-right: 15px;"
    )
  ),
  id = "navbar",
  sidebar = sidebar(
    id = "main_sidebar",
    selectInput("anio", "Año:", choices = NULL, selectize = TRUE),
    selectInput("ooad", "OOAD:", choices = NULL),
    selectInput("unidad_medica", "Unidad Médica:", choices = NULL),
    selectInput("metric_choice", "Métrica a analizar:", choices = metric_choices, selected = "Incidencia")
  ),
  nav_panel(
    title = "Inicio",
    icon = icon("home"),
    inicio_portada_UI("inicio_portada")
  ),
  nav_panel(
    title = "Métricas principales",
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      metric_summary_UI("metric_summary")
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("Curva Temporal"),
        time_chart_UI("time_chart")
      ),
      card(
        full_screen = TRUE,
        card_header("Mapa"),
        map_chart_ui("map")
      )
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header("Gráfico Edad y Sexo"),
        age_sex_graph_UI("age_sex_graph")
      )
    )
  ),
  #nav_panel(
  #  title = "Indicadores Médicos",
  #  layout_column_wrap(
    #  card(
    #   width = 1/3,
    #    full_screen = TRUE,
    #    card_header("Módulo de Pruebas"),
    #    test_mod_UI("")
    #  ),
  #    card(
  #      width = 1,
  #      full_screen = TRUE,
  #      card_header("Indicadores"),
  #      indicadores_table_UI("indicadores_table")
   #   )
  #  )
  #),
  nav_panel(
    title = "Proceso DM y su impacto",
    indicadores_cruce_UI("indicadores_cruce")
  )
)
