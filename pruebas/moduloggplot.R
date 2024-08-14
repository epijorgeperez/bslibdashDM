age_sex_graph_UI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("age_sex_graph"), height = "300px", width = "100%")
}

age_sex_graph_server <- function(id, anio, ooad, unidad_medica) {
  moduleServer(id, function(input, output, session) {
    output$age_sex_graph <- renderPlot({
      # Read the data
      data <- data_censo()

      # Filter the data based on the user's input
      filtered <- data %>%
        filter(Anio == anio(), Nombre_OOAD == ooad(), Nombre_Unidad == unidad_medica()) %>%
        filter(Sexo %in% c(1, 2)) # Only include 'hombre' and 'mujer'
      
      # Create the ggplot graph
      ggplot(filtered, aes(x = Grupo_edad, y = Pacientes_DM, fill = factor(Sexo))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("#35357a", "#fa4664"), labels = c("Hombre", "Mujer")) +
        labs(x = "Grupo de edad", y = "Casos", fill = "Sexo") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(title = "Sexo"))

    })
  })
}