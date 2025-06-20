# modules/age_sex_graph.R

age_sex_graph_UI <- function(id) {
  ns <- NS(id)
  echarts4rOutput(ns("age_sex_graph"), height = "400px", width = "100%")
}

age_sex_graph_server <- function(id, metrica, data_censo, data_consulta, data_incapacidad, data_hosp, data_mortalidad, data_incidencia, anio, ooad, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    output$age_sex_graph <- renderEcharts4r({
      req(metrica(), anio(), unidad_medica())
      
      # Get the corresponding data based on the selected metric
      data <- switch(metrica(),
        "Prevalencia" = data_censo(),
        "Consultas" = data_consulta(),
        "Incapacidades" = data_incapacidad(),
        "Hospitalizaciones" = data_hosp(),
        "Mortalidad" = data_mortalidad(),
        "Incidencia" = data_incidencia()
      )
      
      req(data)

      # Filter the data based on the user's input
      filtered <- data %>% 
        filter(Anio == anio(),  Nombre_Unidad == unidad_medica(), Sexo %in% c(1, 2), Grupo_edad != "Total") #Nombre_OOAD == ooad(), agregar filtro de ooad si es necesario
       
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
        e_bar(Hombre, stack = "grp", itemStyle = list(color = "#3838a3"))%>%
        e_axis_labels(x = "Grupos de edad") |> # axis labels
       # e_title("Casos de Diabetes por sexo y edad") |>  # Add title & subtitle
        e_theme("infographic") |>  # theme
        e_legend(right = 0) |>  # move legend to the bottom
        e_tooltip(trigger = "axis") # tooltip
    })
  })
}