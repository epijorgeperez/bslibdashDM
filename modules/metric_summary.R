# modules/metric_summary.R

metric_summary_UI <- function(id) {
  ns <- NS(id)
  
  create_metric_card <- function(title, metrics) {
    card(
      card_header(title),
      card_body(
        style = "padding: 0.25rem; font-size: 0.9rem; line-height: 1.1;",
        lapply(seq_along(metrics), function(i) {
          div(
            style = paste0("display: flex; justify-content: space-between;", 
                           if(i < length(metrics)) " margin-bottom: 0.1rem;" else ""),
            span(style = "margin-right: 0.5rem;", strong(names(metrics)[i])),
            span(textOutput(ns(metrics[[i]])))
          )
        })
      )
    )
  }

  layout_columns(
    create_metric_card("Incidencia", list(
      "Unidad:" = "incidence",
      "OOAD:" = "incidence_ooad",
      "Nacional:" = "incidence_nac"
    )),
    create_metric_card("Prevalencia", list(
      "Unidad:" = "prevalence",
      "OOAD:" = "prevalence_ooad",
      "Nacional:" = "prevalence_nac"
    )),
    create_metric_card("Consultas", list(
      "Unidad:" = "consultations",
      "OOAD:" = "consultations_ooad",
      "Nacional:" = "consultations_nac"
    )),
    create_metric_card("Hospitalizaciones", list(
      "Unidad:" = "hospitalizaciones",
      "OOAD:" = "hospitalizaciones_ooad",
      "Nacional:" = "hospitalizaciones_nac",
      "Estancia prom.:" = "dias",
      "Estancia OOAD prom.:" = "dias_ooad",
      "Estancia Nac. prom." = "dias_nac"
    )),
    create_metric_card("Mortalidad", list(
      "Unidad:" = "mortality",
      "OOAD:" = "mortality_ooad",
      "Nacional:" = "mortality_nac"
    )),
    create_metric_card("Incapacidades", list(
      "Unidad:" = "disabilities",
      "OOAD:" = "disabilities_ooad",
      "Nacional:" = "disabilities_nac",
      "Promedio:" = "disabilities_prom",
      "Promedio OOAD:" = "disabilities_ooad_prom",
      "Promedio Nac.:" = "disabilities_nac_prom"
    )),
    col_widths = c(2, 2, 2, 2, 2, 2)
  )
}

metric_summary_server <- function(id, totales_anuales, totales_consultas, totales_incap, totales_hosp, totales_incidencia, 
                                  totales_mortalidad, anio, ooad, unidad_medica, metric) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression to filter the 'totales_anuales' data based on the user's input
    prev_data_unidad <- reactive({
      # Accessing the reactive 'totales_anuales' from the server environment
      filtered <- totales_anuales() %>%
        filter(
          Anio == anio(),
          Nombre_OOAD == ooad(),
          Nombre_Unidad == unidad_medica()
        )
      # Return the filtered data
      filtered
    })

    prev_data_nacional <- reactive({
      # Accessing the reactive 'totales_anuales' from the server environment
      filtered <- totales_anuales() %>%
        filter(
          Anio == anio(),
          Nombre_OOAD == "Nacional",
          Nombre_Unidad == "Nacional"
        )
      # Return the filtered data
      filtered
    })



    filtered_consultas <- reactive({
      # Accessing the reactive 'totales_consultas' from the server environment
      filtered_c <- totales_consultas() %>%
        filter(
          #Parametro == "Consulta_MF",
          Anio == anio(),
          Nombre_OOAD == ooad(),
          Nombre_Unidad == unidad_medica()
        )
      # Return the filtered data
      filtered_c
    })

consultas_nac <- reactive({    
      # Accessing the reactive 'totales_consultas' from the server environment
      filtered_c <- totales_consultas() %>%
        filter(
          #Parametro == "Consulta_MF",
          Anio == anio(),
          Nombre_OOAD == "Nacional",
          Nombre_Unidad == "Total Nacional"
        )
      # Return the filtered data
      filtered_c
    })

filtered_incap <- reactive({        
      # Accessing the reactive 'totales_incap' from the server environment
      filtered_i <- totales_incap() %>%
        filter(
          Anio == anio(),
          Nombre_Unidad == unidad_medica()
        )
      # Return the filtered data
      filtered_i
    })

incap_nac <- reactive({
      # Accessing the reactive 'totales_incap' from the server environment
      filtered_i <- totales_incap() %>%
        filter(
          Anio == anio(),
          Nombre_Unidad == "Nacional"
        )
      # Return the filtered data
      filtered_i
    })  

filtered_incid <- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_in <- totales_incidencia() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      Nombre_Unidad == unidad_medica()
    )
  filtered_in
})

incid_nac <- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_in_nac <- totales_incidencia() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      Nombre_Unidad == unidad_medica()
    )
  filtered_in_nac
})

filtered_hosp <- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_h <- totales_hosp() %>%
    filter(
      Anio == anio(),
      Nombre_OOAD == ooad(),
      Nombre_Unidad == unidad_medica()
    )
  filtered_h
})

hosp_nac <- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_h <- totales_hosp() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      Nombre_Unidad == "Total Nacional"
    )
  filtered_h
})

filtered_dias_est<- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_d <- totales_dias_estancia() %>%
    filter(
      Anio == anio(),
      Nombre_OOAD == ooad(),
      Nombre_Unidad == unidad_medica()
    )
  filtered_d
})

filtered_dias_est_nac <- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_dn <- totales_dias_estancia() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      Nombre_Unidad == "Total Nacional"
    )
  filtered_dn
})

filtered_mort <- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_m <- totales_mortalidad() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      Nombre_Unidad == unidad_medica()#DUMMY CORREGIR UNA VEZ QUE SE TRABAJEN AGREGADOS POR OOAD Y NACIONAL
    )
  filtered_m
})
mort_nac <- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_m <- totales_mortalidad() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      Nombre_Unidad == unidad_medica()#DUMMY CORREGIR UNA VEZ QUE SE TRABAJEN AGREGADOS POR OOAD Y NACIONAL
    )
  filtered_m
})

# Add OOAD level filtering and rendering for each metric

prev_data_ooad <- reactive({
      # Accessing the reactive 'totales_anuales' from the server environment
      filtered <- totales_anuales() %>%
        filter(
          Anio == anio(),
          Nombre_OOAD == ooad(),
          Nombre_Unidad == "OOAD"
        )
      # Return the filtered data
      filtered
    })

filtered_consultas_ooad <- reactive({
  totales_consultas() %>%
    filter(
      Anio == anio(),
      Nombre_OOAD == ooad(),
      Nombre_Unidad == "Total OOAD"
    )
})

filtered_incap_ooad <- reactive({
  totales_incap() %>%
    filter(
      Anio == anio(),
      nombre_ooad == ooad(),
      Nombre_Unidad == "OOAD"
    )
})

filtered_hosp_ooad <- reactive({
  totales_hosp() %>%
    filter(
      Anio == anio(),
      Nombre_OOAD == ooad(),
      Nombre_Unidad == "Total OOAD"
    )
})

filtered_dias_est_ooad <- reactive({
  totales_dias_estancia() %>%
    filter(
      Anio == anio(),
      Nombre_OOAD == ooad(),
      Nombre_Unidad == "Total OOAD"
    )
})

#PARA INICIDENCIA Y MORTALIDAD ES NECESARIO AGREGAR EL ACUMULADO POR OOAD Y NACIONAL EN 
#GLOBAL.R AL TRANSFORMAR LA BASE DE DATOS YA QUE NO VIENEN ESAS CATEGORÍAS COMO EN EL RESTO
#DE LAS BASES DE DATOS 

filtered_incid_ooad <- reactive({
  totales_incidencia() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      Nombre_Unidad == unidad_medica() #DUMMY CORREGIR UNA VEZ QUE SE TRABAJEN AGREGADOS POR OOAD Y NACIONAL
    )
})

filtered_mort_ooad <- reactive({
  totales_mortalidad() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      Nombre_Unidad == unidad_medica()#DUMMY CORREGIR UNA VEZ QUE SE TRABAJEN AGREGADOS POR OOAD Y NACIONAL
    )
})


#Incidencia
    output$incidence <- renderText({ 
      paste0(formatC(filtered_incid()$Dato, big.mark = ",", format = "f", digits = 1), " x 100,000 dh") 
    })
    output$incidence_ooad <- renderText({ 
      paste0(formatC(filtered_incid_ooad()$Dato, big.mark = ",", format = "f", digits = 1), " x 100,000 dh") 
    })
    output$incidence_nac <- renderText({ 
      paste0(formatC(incid_nac()$Dato, big.mark = ",", format = "f", digits = 1), " x 100,000 dh") 
    })
#Prevalencia
    output$prevalence <-  renderText({
      paste0(formatC(prev_data_unidad()$Dato, format = "f", digits = 2), "%")
    })
    output$prevalence_ooad <-  renderText({
      paste0(formatC(prev_data_ooad()$Dato, format = "f", digits = 2), "%")
    })
    output$prevalence_nac <-  renderText({
      paste0(formatC(prev_data_nacional()$Dato, format = "f", digits = 2), "%")
    })
#Consultas
    output$consultations <- renderText({ 
      paste0(formatC(filtered_consultas()$Dato2, big.mark = ",", format = "f", digits = 0), " x 1,000 dh") 
    })
    output$consultations_ooad <- renderText({ 
      paste0(formatC(filtered_consultas_ooad()$Dato2, big.mark = ",", format = "f", digits = 0), " x 1,000 dh") 
    })
    output$consultations_nac <- renderText({ 
      paste0(formatC(consultas_nac()$Dato2, big.mark = ",", format = "f", digits = 0), " x 1,000 dh")
    })
#Hospitalizaciones    
    output$hospitalizaciones <- renderText({ 
      paste0(formatC(filtered_hosp()$Dato2, big.mark = ",", format = "f", digits = 0) , " x 100,000 dh")
    })
    output$hospitalizaciones_ooad <- renderText({ 
      paste0(formatC(filtered_hosp_ooad()$Dato2, big.mark = ",", format = "f", digits = 0) , " x 100,000 dh")
    })
    output$hospitalizaciones_nac <- renderText({ 
      paste0(formatC(hosp_nac()$Dato2, big.mark = ",", format = "f", digits = 0) , " x 100,000 dh")
    })
    output$dias <- renderText({ 
      paste0(formatC(filtered_dias_est()$Dato, big.mark = ",", format = "f", digits = 1), " días") 
    })
    output$dias_ooad <- renderText({ 
      paste0(formatC(filtered_dias_est_ooad()$Dato, big.mark = ",", format = "f", digits = 1), " días") 
    })
    output$dias_nac <- renderText({
      paste0(formatC(filtered_dias_est_nac()$Dato, big.mark = ",", format = "f", digits = 1), " días") 
    })
#Mortalidad
    output$mortality <- renderText({ 
      paste0(formatC(filtered_mort()$Dato, big.mark = ",", format = "f", digits = 1), " x 100,000 dh" )
    })
    output$mortality_ooad <- renderText({ 
      paste0(formatC(filtered_mort_ooad()$Dato, big.mark = ",", format = "f", digits = 1), " x 100,000 dh" )
    })
    output$mortality_nac <- renderText({ 
      paste0(formatC(mort_nac()$Dato, big.mark = ",", format = "f", digits = 1), " x 100,000 dh" )
    })
#Incapacidades
    output$disabilities <- renderText({ 
      paste0(formatC(filtered_incap()$Dato, big.mark = ",", format = "f", digits = 1), " días por 100 DH RT") 
    })
    output$disabilities_ooad <- renderText({ 
      paste0(formatC(filtered_incap_ooad()$Dato, big.mark = ",", format = "f", digits = 1), " días por 100 DH RT") 
    })
    output$disabilities_nac <- renderText({ 
      paste0(formatC(incap_nac()$Dato, big.mark = ",", format = "f", digits = 1), " días por 100 DH RT") 
    })
    output$disabilities_prom <- renderText({ 
      paste0(formatC(filtered_incap()$Dato_prom, big.mark = ",", format = "f", digits = 0), " días por inc.") 
    })
    output$disabilities_ooad_prom <- renderText({ 
      paste0(formatC(filtered_incap_ooad()$Dato_prom, big.mark = ",", format = "f", digits = 0), " días por inc.") 
    })
    output$disabilities_nac_prom <- renderText({ 
      paste0(formatC(incap_nac()$Dato_prom, big.mark = ",", format = "f", digits = 0), " días por inc.") 
    })

  })}