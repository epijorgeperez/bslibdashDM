library(readr)
library(dplyr)
library(stringr)
library(echarts4r)
#totales_consultas
data <- read_csv("data/tb_consulta_DM.csv")
  totales_consultas<-  data %>% 
      filter (Parametro == 'Consulta_MF', Sexo == 0, Grupo_edad =="Total") %>%
      group_by(Parametro, Anio, Nombre_OOAD, Nombre_Unidad, Sexo) %>%
      summarise(Dato = sum(Dato, na.rm = TRUE),
                .groups = 'drop')

      filtered_c <- totales_consultas %>%
        filter(
          Anio %in% c(2024:2020),
          Nombre_OOAD == 'Jalisco',
          Nombre_Unidad == 'UMF 168 Tepatitlán',
          Sexo == 0
        )

#totales_incap

poblacion <- read_csv("data/tb_poblacion.csv")
cuums <- read_csv("data/cuums_maestro.csv")

poblacion_rt <- poblacion %>% filter(Parametro == "PAU RT") %>%
                left_join(cuums %>% select(ClavePresupuestal, cve_prei = UnidadInformacionPREI),
                          by = c("Cve_Presupuestal" = "ClavePresupuestal")) %>%
                mutate(cve_prei = case_when(Nombre_Unidad == "Jalisco" ~ "14",
                                            Nombre_Unidad == "Nacional" ~ "99",
                                            TRUE ~ cve_prei))

poblacion_totales_rt <- poblacion_rt %>%
  filter(Sexo == 0) %>%
  group_by(Anio, Cve_Presupuestal, Nombre_Unidad, cve_prei)%>%
  summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Cve_Presupuestal = as.character(Cve_Presupuestal))
  names(poblacion_totales_rt) <- tolower(names(poblacion_totales_rt))

poblacion_gpoedad_rt <- poblacion_rt %>%
    filter(Sexo != 0, Grupo_edad != "NI") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad, cve_prei)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')




data <- read_csv("data/tb_dm_incap.csv")
    data <- data %>% 
      rename(Anio = PERIODO, Nombre_Unidad = descnivel) %>%
      group_by(Anio, Nombre_Unidad, NIVEL) %>%
      summarise(ndias = sum(NDIAS, na.rm = TRUE), 
                Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                .groups = 'drop') %>%
      mutate(NIVEL = as.character(NIVEL)) %>%
      left_join(poblacion_totales_rt %>% select(cve_prei, anio, totales_poblacion), 
                by = c('NIVEL' = 'cve_prei', "Anio" = "anio")) %>%
      mutate(Dato = ndias/totales_poblacion*100) %>%
      mutate(Nombre_Unidad = case_when(is.na(Nombre_Unidad) ~ NA_character_,
                                       Nombre_Unidad == "14 Jalisco" ~ "Jalisco",
                                       Nombre_Unidad == "99 Nacional" ~ "Nacional",
                                       TRUE ~ str_trim(str_extract(Nombre_Unidad, "(?<=\\s).*"))))
    data


      filtered_i <- totales_incap %>%
        filter(
          PERIODO %in% c(2022:2014),
          descnivel =='UMF 168 Tepatitlán'
        )



data <- read_csv("data/tb_dm_incap.csv") %>%
    rename(Anio = PERIODO, Nombre_Unidad = descnivel, Grupo_edad = descgedad, Sexo = TIP_SEXO)%>%
    group_by(Anio, Nombre_Unidad, NIVEL, Grupo_edad, Sexo) %>%
      summarise(Dato = sum(NDIAS, na.rm = TRUE), 
                Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                .groups = 'drop') %>%
      mutate(Nombre_Unidad = case_when(is.na(Nombre_Unidad) ~ NA_character_,
            Nombre_Unidad == "14 Jalisco" ~ "Jalisco",
            Nombre_Unidad == "99 Nacional" ~ "Nacional",
            TRUE ~ str_trim(str_extract(Nombre_Unidad, "(?<=\\s).*"))))
data


filtered <- data %>% 
        filter(Anio == 2023,  Nombre_Unidad == "HGZMF 9 Cd. Guzmán", Sexo %in% c(1, 2)) #Nombre_OOAD == ooad(), agregar filtro de ooad si es necesario
       
      # To stack by sex, ensure 'Sexo' is a factor with the desired level names
      filtered$Sexo <- factor(filtered$Sexo, levels = c(1, 2), labels = c("Hombre", "Mujer"))
  
      # Group and summarise data
      data_grouped <- filtered %>%
        group_by(Grupo_edad, Sexo) %>%
        summarise(Casos = sum(Dato, na.rm = TRUE), .groups = 'drop')

      # Create the echarts4r graph with stacked bars
      data_grouped %>%
        pivot_wider(names_from = Sexo, values_from = Casos) %>%
        #mutate(total = Hombre + Mujer) %>%
        e_charts(Grupo_edad)%>%
        e_bar(Mujer, stack = "grp", itemStyle = list(color = "pink")) %>%
        e_bar(Hombre, stack = "grp", itemStyle = list(color = "#3838a3")) %>%
        e_axis_labels(x = "Grupos de edad") |> # axis labels
       # e_title("Casos de Diabetes por sexo y edad") |>  # Add title & subtitle
        e_theme("infographic") |>  # theme
        e_legend(right = 0) |>  # move legend to the bottom
        e_tooltip(trigger = "axis") # tooltip




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