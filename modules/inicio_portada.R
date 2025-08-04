# modules/inicio_portada.R

inicio_portada_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS y JavaScript para ocultar el sidebar en la página de inicio
    tags$head(
      tags$style(HTML("
        .sidebar-hidden .bslib-sidebar-layout > .sidebar {
          display: none !important;
        }
        .sidebar-hidden .bslib-sidebar-layout > .main {
          margin-left: 0 !important;
          width: 100% !important;
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {
          function updateSidebar() {
            var activeTab = $('.nav-link.active').text().trim();
            if(activeTab === 'Inicio') {
              $('body').addClass('sidebar-hidden');
            } else {
              $('body').removeClass('sidebar-hidden');
            }
          }
          updateSidebar();
          $('.nav-link').on('click', function() {
            setTimeout(updateSidebar, 100);
          });
        });
      "))
    ),
    
    # Header Section
    div(
      style = "background: linear-gradient(135deg, #006456 0%, #004c44 100%); 
               color: white; padding: 2rem; margin-bottom: 2rem; border-radius: 10px;",
      div(
        style = "text-align: center;",
        h1("Diabetes en el IMSS", 
           style = "font-size: 2.5rem; margin-bottom: 1rem; font-weight: bold;"),
        h3("Análisis Epidemiológico Integral del Comportamiento de la Diabetes en el IMSS",
           style = "font-size: 1.3rem; font-weight: normal; opacity: 0.9;")
      )
    ),
    
    # Main Content Container
    div(
      class = "container-fluid px-4",
      
      # Objetivo Section
      card(
        card_header(
          icon("bullseye"), " Objetivo del Tablero",
          style = "background-color: #006456; color: white; font-weight: bold;"
        ),
        card_body(
          div(
            style = "font-size: 1.1rem; line-height: 1.6;",
            p("Este tablero tiene como objetivo principal ", strong("conocer la situación actual de la Diabetes en el IMSS"), 
              " desde el punto de vista de su comportamiento epidemiológico, analizando indicadores de impacto como la morbilidad y mortalidad 
              en las dimensiones clásicas de:", style = "margin-bottom: 1rem;"),
            div(
              style = "display: flex; justify-content: space-around; margin: 1.5rem 0;",
              div(
                style = "text-align: center; padding: 1rem; background-color: #f8f9fa; border-radius: 8px; flex: 1; margin: 0 0.5rem;",
                icon("clock", style = "font-size: 2rem; color: #006456; margin-bottom: 0.5rem;"),
                br(),
                strong("TIEMPO", style = "color: #006456;")
              ),
              div(
                style = "text-align: center; padding: 1rem; background-color: #f8f9fa; border-radius: 8px; flex: 1; margin: 0 0.5rem;",
                icon("map-marker-alt", style = "font-size: 2rem; color: #006456; margin-bottom: 0.5rem;"),
                br(),
                strong("LUGAR", style = "color: #006456;")
              ),
              div(
                style = "text-align: center; padding: 1rem; background-color: #f8f9fa; border-radius: 8px; flex: 1; margin: 0 0.5rem;",
                icon("users", style = "font-size: 2rem; color: #006456; margin-bottom: 0.5rem;"),
                br(),
                strong("PERSONA", style = "color: #006456;")
              )
            ),
            p("Adicionalmente, el tablero presenta métricas del proceso salud-enfermedad de la diabetes, 
              reconociendo que es uno de los procesos con ", strong("mayor impacto en la población derechohabiente del IMSS."))
          )
        )
      ),
      
      br(),
      
      # Content Structure and Navigation Section
      div(
        class = "row mb-3",
        div(
          class = "col-md-6",
          card(
            card_header(
              icon("sitemap"), " Estructura del Contenido",
              style = "background-color: #AE851E; color: white; font-weight: bold;"
            ),
            card_body(
              h5(icon("chart-bar"), " Métricas Principales", style = "color: #AE851E; margin-bottom: 1rem;"),
              p("Panorama integral de las métricas epidemiológicas clave:"),
              tags$ul(
                tags$li("Resumen ejecutivo de indicadores"),
                tags$li("Análisis temporal de tendencias"),
                tags$li("Distribución geográfica (mapas)"),
                tags$li("Análisis demográfico por edad y sexo")
              ),
              hr(),
              h5(icon("project-diagram"), " Proceso DM y su Impacto", style = "color: #AE851E; margin-bottom: 1rem;"),
              p("Análisis avanzado de indicadores específicos:"),
              tags$ul(
                tags$li("Evolución temporal de indicadores"),
                tags$li("Análisis de correlaciones"),
                tags$li("Comparaciones entre unidades")
              )
            )
          )
        ),
        div(
          class = "col-md-6",
          card(
            card_header(
              icon("compass"), " Guía de Navegación",
              style = "background-color: #A02042; color: white; font-weight: bold;"
            ),
            card_body(
              h5(icon("filter"), " Controles de Filtrado", style = "color: #A02042; margin-bottom: 1rem;"),
              p("Utiliza el panel lateral para filtrar los datos:"),
              tags$ul(
                tags$li(strong("Año:"), " Selecciona el periodo de análisis"),
                tags$li(strong("OOAD:"), " Filtra por Órgano de Operación Administrativa Desconcentrada"),
                tags$li(strong("Unidad Médica:"), " Especifica la unidad de interés"),
                tags$li(strong("Métrica:"), " Selecciona el indicador a analizar")
              ),
              hr(),
              h5(icon("info-circle"), " Interpretación", style = "color: #A02042; margin-bottom: 1rem;"),
              p("Los datos se presentan en tres niveles de comparación:"),
              tags$ul(
                tags$li(strong("Unidad:"), " Nivel específico seleccionado"),
                tags$li(strong("OOAD:"), " Nivel agregado regional"),
                tags$li(strong("Nacional:"), " Nivel de referencia institucional")
              )
            )
          )
        )
      ),
      
      # Metrics Documentation Section
      card(
        card_header(
          icon("calculator"), " Documentación de Métricas e Indicadores",
          style = "background-color: #691A32; color: white; font-weight: bold;"
        ),
                 card_body(
           div(
             class = "row",
             div(
               class = "col-md-6",
               h5(icon("plus-circle"), " Incidencia", style = "color: #691A32; margin-bottom: 1rem;"),
               div(
                 style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                 p(strong("Definición:"), " Casos nuevos de diabetes diagnosticados"),
                 p(strong("Fórmula:"), " (Casos nuevos / Población en riesgo) × 100,000"),
                 p(strong("Unidad:"), " Casos por 100,000 derechohabientes"),
                 p(strong("Fuente:"), " Morbi_Diabetes")
               ),
               h5(icon("percentage"), " Prevalencia", style = "color: #691A32; margin-bottom: 1rem;"),
               div(
                 style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                 p(strong("Definición:"), " Porcentaje de derechohabientes con diagnóstico de Diabetes Mellitus"),
                 p(strong("Fórmula:"), " (Pacientes DM / Población Adscrita a Médico Familiar) × 100"),
                 p(strong("Unidad:"), " Porcentaje (%)"),
                 p(strong("Fuente:"), " tb_censo_DM")
               ),
               h5(icon("stethoscope"), " Consultas", style = "color: #691A32; margin-bottom: 1rem;"),
               div(
                 style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                 p(strong("Definición:"), " Consultas de medicina familiar por diabetes"),
                 p(strong("Fórmula:"), " (Total consultas / Población total) × 1,000"),
                 p(strong("Unidad:"), " Consultas por 1,000 derechohabientes"),
                 p(strong("Fuente:"), " tb_consulta_dm")
               ),
               h5(icon("clock"), " Días de Estancia Promedio", style = "color: #691A32; margin-bottom: 1rem;"),
               div(
                 style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                 p(strong("Definición:"), " Promedio de días de estancia hospitalaria por diabetes"),
                 p(strong("Fórmula:"), " mean(DiasEstancia_DM_Adsc)"),
                 p(strong("Unidad:"), " Días de estancia promedio por hospitalización"),
                 p(strong("Fuente:"), " tb_egreso_dm")
               )
             ),
             div(
               class = "col-md-6",
               h5(icon("hospital"), " Hospitalizaciones", style = "color: #691A32; margin-bottom: 1rem;"),
               div(
                 style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                 p(strong("Definición:"), " Egresos hospitalarios por diabetes"),
                 p(strong("Fórmula:"), " (Egresos DM / Población total) × 100,000"),
                 p(strong("Unidad:"), " Egresos por 100,000 derechohabientes"),
                 p(strong("Fuente:"), " tb_egreso_dm")
               ),
               h5(icon("skull-crossbones"), " Mortalidad", style = "color: #691A32; margin-bottom: 1rem;"),
               div(
                 style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                 p(strong("Definición:"), " Defunciones por diabetes como causa básica"),
                 p(strong("Fórmula:"), " (Defunciones / Población total) × 100,000"),
                 p(strong("Unidad:"), " Defunciones por 100,000 derechohabientes"),
                 p(strong("Fuente:"), " Morta_Diabetes")
               ),
               h5(icon("bed"), " Incapacidades", style = "color: #691A32; margin-bottom: 1rem;"),
               div(
                 style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                 p(strong("Definición:"), " Días de incapacidad temporal por diabetes por cada 100 trabajadores asegurados"),
                 p(strong("Fórmula:"), " (Días incapacidad / Población RT) × 100"),
                 p(strong("Unidad:"), " Días por 100 derechohabientes en RT"),
                 p(strong("Fuente:"), " tb_dm_incap")
               ),
               h5(icon("calendar-alt"), " Días por Incapacidad Promedio", style = "color: #691A32; margin-bottom: 1rem;"),
               div(
                 style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                 p(strong("Definición:"), " Promedio de duración en días por cada incapacidad temporal emitida por DM"),
                 p(strong("Fórmula:"), " (Total NDIAS / Total FREC)"),
                 p(strong("Unidad:"), " Días promedio por incapacidad"),
                 p(strong("Fuente:"), " tb_dm_incap")
               )
             )
           )
         )
             ),

      # Advanced Analytics Section - Proceso DM y su Impacto
      card(
        card_header(
          icon("project-diagram"), " Análisis Avanzado: Proceso DM y su Impacto",
          style = "background-color: #AE851E; color: white; font-weight: bold;"
        ),
        card_body(
          div(
            class = "row",
            div(
              class = "col-md-6",
              h5(icon("chart-line"), " Evolución Temporal", style = "color: #AE851E; margin-bottom: 1rem;"),
              p("Seguimiento de indicadores específicos a lo largo del tiempo:"),
              tags$ul(
                tags$li("Comparación entre Nacional, OOAD y Unidad Médica"),
                tags$li("Visualización interactiva con dygraphs"),
                tags$li("Datos mensuales de múltiples años")
              ),
              
              h5(icon("balance-scale"), " División de Indicadores", style = "color: #AE851E; margin-bottom: 1rem;"),
              div(
                style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px;",
                p(strong("Productividad (Eje X):"), " DM 01, DM 02, DM 04, DM 05, DM 06"),
                p(style = "font-size: 0.9rem; color: #6c757d;", "Indicadores de proceso y eficiencia operativa"),
                p(strong("Impacto (Eje Y):"), " DM 03, DM 07, DM 08, DM 09"),
                p(style = "font-size: 0.9rem; color: #6c757d;", "Indicadores de resultados en salud")
              )
            ),
            div(
              class = "col-md-6",
              h5(icon("crosshairs"), " Análisis de Cuadrantes", style = "color: #AE851E; margin-bottom: 1rem;"),
              div(
                style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px; margin-bottom: 1rem;",
                p(strong("Líneas de Referencia:"), " Dividen el gráfico en 4 cuadrantes"),
                tags$ul(
                  style = "font-size: 0.9rem;",
                  tags$li(strong("Rojas:"), " Valores de referencia oficiales"),
                  tags$li(strong("Grises:"), " Valores promedio (cuando no hay referencia)")
                ),
                p(strong("Cuadrante Meta:"), " Zona verde que indica la combinación óptima según los valores esperados")
              ),
              
              h5(icon("calculator"), " Estadística de Correlación", style = "color: #AE851E; margin-bottom: 1rem;"),
              div(
                style = "background-color: #f8f9fa; padding: 1rem; border-radius: 5px;",
                p(strong("Coeficiente r:"), " Mide la fuerza de la relación entre indicadores"),
                tags$ul(
                  style = "font-size: 0.9rem;",
                  tags$li("Fuerte: |r| ≥ 0.7"),
                  tags$li("Moderada: 0.4 ≤ |r| < 0.7"),
                  tags$li("Débil: 0.2 ≤ |r| < 0.4"),
                  tags$li("Muy Débil: |r| < 0.2")
                ),
                p(strong("Significancia:"), " p < 0.05 indica relación estadísticamente significativa")
              )
            )
          )
        )
      ),
      
      # Forecasting Section
      card(
        card_header(
          icon("chart-line"), " Módulo de Pronósticos",
          style = "background-color: #A02042; color: white; font-weight: bold;"
        ),
        card_body(
          div(
            class = "row",
            div(
              class = "col-md-6",
              h5(icon("robot"), " Selección Automática", style = "color: #A02042; margin-bottom: 1rem;"),
              p("El sistema compara automáticamente métodos ETS y ARIMA, seleccionando el mejor basado en AIC."),
              
              h5(icon("cogs"), " Métodos Disponibles", style = "color: #A02042; margin-bottom: 1rem;"),
              tags$ul(
                tags$li(strong("Automático:"), " Selección óptima por AIC"),
                tags$li(strong("ETS:"), " Suavizado exponencial"),
                tags$li(strong("ARIMA:"), " Modelos autoregresivos"),
                tags$li(strong("Naive:"), " Modelo base de comparación")
              )
            ),
            div(
              class = "col-md-6",
              h5(icon("calendar-alt"), " Configuración", style = "color: #A02042; margin-bottom: 1rem;"),
              tags$ul(
                tags$li("Períodos a pronosticar: 1-24 años"),
                tags$li("Niveles de confianza: 80%, 90%, 95%"),
                tags$li("Datos filtrados automáticamente (solo años completos)")
              ),
              
              h5(icon("chart-area"), " Resultados", style = "color: #A02042; margin-bottom: 1rem;"),
              tags$ul(
                tags$li("Gráfico interactivo con intervalos de confianza"),
                tags$li("Métricas de precisión (RMSE, MAE, MAPE)"),
                tags$li("Tabla de valores pronosticados")
              )
            )
          )
        )
      ),
      

      br(),
      
      # Footer
      div(
        style = "text-align: center; padding: 1.5rem; background-color: #f8f9fa; border-radius: 5px; margin-top: 2rem;",
        p(strong("CIIMSS - Centro de Inteligencia en Salud del IMSS"), 
          style = "color: #006456; margin-bottom: 0.5rem;"),
        p("Tablero desarrollado para el análisis integral del comportamiento epidemiológico de la diabetes",
          style = "color: #6c757d; font-size: 0.9rem;")
      )
    )
  )
}

inicio_portada_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Este módulo es principalmente informativo, no requiere lógica de servidor
    # Se puede agregar lógica adicional si se necesitan elementos interactivos
  })
} 