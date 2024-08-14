library(readr)
library(dplyr)

#totales_consultas
data <- read_csv("data/tb_consulta_DM.csv")
  totales_consultas<-  data %>% 
      filter (Parametro == 'Consulta_MF') %>%
      group_by(Parametro, Anio, Nombre_OOAD, Nombre_Unidad, Sexo) %>%
      summarise(Dato = sum(Dato, na.rm = TRUE),
                .groups = 'drop')

      filtered_c <- totales_consultas %>%
        filter(
          Anio %in% c(2022:2014),
          Nombre_OOAD == 'Jalisco',
          Nombre_Unidad == 'UMF 168 Tepatitlán',
          Sexo == 0
        )

#totales_incap
data <- read_csv("data/tb_dm_incap.csv")
  totales_incap<-  data %>%
      group_by(PERIODO, descnivel) %>%
      summarise(Dato = sum(NDIAS, na.rm = TRUE), 
                Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                .groups = 'drop')

      filtered_i <- totales_incap %>%
        filter(
          PERIODO %in% c(2022:2014),
          descnivel =='UMF 168 Tepatitlán'
        )







### modulo time_chart original




time_chart_UI <- function(id) {
  ns <- NS(id)
  dygraphOutput(ns("time_chart"), height = "300px", width = "100%")
}

time_chart_server <- function(id, totales_anuales, ooad, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    output$time_chart <- renderDygraph({
      # Read the data
      data <- totales_anuales()

      # Filter the data based on the user's input
      filtered <- data %>%
        filter(Nombre_OOAD == ooad(), Nombre_Unidad == unidad_medica())%>%
  mutate(Anio = ymd(paste(Anio, "-01-01", sep = ""))) %>%
  select(Anio, Prevalencia_DM_porc)  # Select only the necessary columns

# Creating the dygraph
dy_graph <- dygraph(filtered, xlab = "Año", ylab = "Prevalencia") %>%
  dySeries('Prevalencia_DM_porc', label = "Prevalencia de Diabetes") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyAxis("y", label = "Prevalencia (%)") %>%
  dyAxis("x", label = "Año") %>%
  dyBarChart()

dy_graph
    })
  })
}