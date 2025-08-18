source("modules/inicio_portada.R")
source("modules/metric_summary.R")
source("modules/age_sex_graph.R")
source("modules/time_chart.R")
source("modules/map_chart.R")
source("modules/test_mod.R")
#source("modules/indicadores_table.R")
source("modules/indicadores_cruce.R")
source("modules/simple_forecasting.R")

function(input, output, session) {
  
  observeEvent(totales_anuales(), {
    updateSelectInput(session, "anio", choices = c(2025:2014), selected = 2024)
    updateSelectInput(session, "ooad", choices = sort(unique(totales_anuales()$Nombre_OOAD[totales_anuales()$Nombre_OOAD != "Nacional"])))
    updateSelectInput(session, "unidad_medica", choices = sort(unique(totales_anuales()$Nombre_Unidad)))
  })

  anio <- reactive({ input$anio })
  ooad <- reactive({ input$ooad })
  unidad_medica <- reactive({ input$unidad_medica })
  metrica <- reactive({ input$metric_choice })

  # Módulo de inicio/portada
  inicio_portada_server("inicio_portada")

  metric_summary_server("metric_summary", totales_anuales, totales_consultas, totales_incap, totales_hosp, 
                        totales_incidencia, totales_mortalidad, anio, ooad, unidad_medica, metrica)

  time_chart_server("time_chart", metrica, totales_anuales, totales_consultas, totales_incap, totales_hosp, 
                    totales_mortalidad, totales_incidencia, ooad, unidad_medica)

  age_sex_graph_server("age_sex_graph", metrica, data_censo, data_consulta, data_incapacidad, data_hosp, 
                       data_mortalidad, data_incidencia, anio, ooad, unidad_medica)

  map_chart_server("map", metrica, totales_anuales, totales_consultas, totales_incap, totales_hosp, 
                   totales_mortalidad, totales_incidencia, citiesmx, data_censo_maestro, jalisco_shape, anio)

  test_mod_server("test_mod", metrica, totales_anuales, totales_consultas, totales_incap, totales_hosp, 
                  data_censo, data_consulta, data_hosp, data_incapacidad, data_mortalidad)

  #indicadores_table_server("indicadores_table", data_indicadores, anio, unidad_medica)

  indicadores_cruce_server("indicadores_cruce", data_indicadores, anio, unidad_medica)

  simple_forecasting_server("forecasting", metrica, 
                            # Yearly data
                            totales_anuales, totales_consultas, totales_incap, totales_hosp, 
                            totales_mortalidad, totales_incidencia,
                            # Monthly data  
                            totales_anuales_mensuales, totales_consultas_mensuales, totales_incap_mensuales, 
                            totales_hosp_mensuales, totales_mortalidad_mensuales, totales_incidencia_mensuales,
                            # Other parameters
                            ooad, unidad_medica)
  
  # Uncomment if you're using a database connection
  # session$onSessionEnded(function() {
  #   dbDisconnect(connection_details)
  # })
}
