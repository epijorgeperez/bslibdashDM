source("modules/demo.R")

metric_summary_demo()

library(readr)
library(dplyr)
#totales_anuales
    data <- read_csv("data/tb_censo_DM.csv")
    data <- data %>%
      mutate(Nombre_Unidad = ifelse(nchar(Cve_Presupuestal) < 3 & Cve_Presupuestal != "00", "OOAD", Nombre_Unidad)) %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
      summarise(Pacientes_DM = sum(Pacientes_DM, na.rm = TRUE), 
                PAMF = sum(PAMF, na.rm = TRUE),
                Prevalencia_DM_porc = Pacientes_DM / PAMF * 100,
                .groups = 'drop')




filtered <- data %>% 
  filter(Nombre_OOAD == 'Jalisco', 
         Nombre_Unidad == 'UMF 168 Tepatitlán') %>%
  mutate(Anio = ymd(paste(Anio, "-01-01", sep = ""))) %>%
  select(Anio, Prevalencia_DM)  # Select only the necessary columns

# Creating the dygraph
dy_graph <- dygraph(filtered, xlab = "Año", ylab = "Prevalencia") %>%
  dySeries('Prevalencia_DM', label = "Prevalencia de Diabetes") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyAxis("y", label = "Prevalencia (%)") %>%
  dyAxis("x", label = "Año") %>%
  dyBarChart()

dy_graph


data <- read_csv("data/tb_censo_DM.csv")
    data %>% filter(Nombre_Unidad == "Jalisco") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
      summarise(Pacientes_DM = sum(Pacientes_DM, na.rm = TRUE), 
                PAMF = sum(PAMF, na.rm = TRUE),
                Prevalencia_DM_porc = Pacientes_DM / PAMF * 100,
                .groups = 'drop')

library(echarts4r)
library(dplyr)
library(echarts4r.maps)
data("cities")

censo_maestro <- read_csv("data/CUMMS_MAESTRO.csv")
citiesmx <- read_csv("data/citiesmx.csv")

#data <- totales_anuales()
data <- data %>%
  left_join(censo_maestro %>% select(DenominacionUnidad, Localidad), by = c("Nombre_Unidad" = "DenominacionUnidad")) %>%
  left_join(citiesmx, by = c("Localidad" = "city"))

data %>% 
  #filter(country == "MX") %>% 
  #mutate(val = runif(n(), 1, 3)) %>% 
  e_charts(lon) %>%
  em_map("Mexico") %>% 
  e_geo(map = "Mexico") %>% 
  e_scatter(lat, Prevalencia_DM_porc, coord_system = "geo", scale = NULL) %>% 
  e_visual_map(min = 1, max = 15)


# age_sex_graph echarts4r
data <- read_csv("data/tb_censo_DM.csv")

filtered <- data %>% filter(Anio == "2022", Nombre_OOAD == "Jalisco", Nombre_Unidad == "UMF 168 Tepatitlán")

filtered <- filtered %>% filter(Sexo %in% c(1,2))

data_grouped <- filtered %>% group_by(Grupo_edad, Sexo) %>%
                summarise(Casos = sum(Pacientes_DM, na.rm = TRUE))%>%
                ungroup
data_grouped$Sexo <- factor(data_grouped$Sexo, levels = c(1, 2), labels = c("Hombre", "Mujer"))

data_grouped %>%
  e_charts(Grupo_edad) %>%
  e_bar(Casos, stack = "Hombre") %>%
  e_bar(Casos, stack = "Mujer") %>%
  e_color(c("#35357a", "#fa4664")) %>%
  e_legend(show = TRUE) %>%
  e_tooltip(trigger = "axis", axis_pointer = list(type = "shadow")) %>%
  e_title("Distribución por Edad y Sexo", subtitle = "Casos de Diabetes") %>%
  e_x_axis(name = "Grupo de Edad") %>%
  e_y_axis(name = "Casos") %>%
  e_theme("walden")


mtcars |>
  tibble::rownames_to_column("model") |>
  mutate(total = mpg + qsec) |>
  arrange(desc(total)) |>
  e_charts(model) |>
  e_bar(mpg, stack = "grp")

library(tidyr)
data_grouped %>%
  pivot_wider(names_from = Sexo, values_from = Casos) %>%
  mutate(total = Hombre + Mujer) %>%
  e_charts(Grupo_edad)%>%
  e_bar(Mujer, stack = "grp") %>%
  e_bar(Hombre, stack = "grp")%>%
  e_axis_labels(x = "Grupos de edad") |> # axis labels
  e_title("Casos de Diabetes por sexo y edad") |>  # Add title & subtitle
  e_theme("infographic") |>  # theme
  e_legend(right = 0) |>  # move legend to the bottom
  e_tooltip(trigger = "axis") # tooltip




v <- LETTERS[1:10]
 matrix <- data.frame(
   x = sample(v, 300, replace = TRUE),
   y = sample(v, 300, replace = TRUE),
   z = rnorm(300, 10, 1),
   color = rnorm(300, 10, 1),
   size = rnorm(300, 10, 1),
   stringsAsFactors = FALSE) |>
  dplyr::group_by(x, y) |>
   dplyr::summarise(
     z = sum(z),
     color = sum(color),
     size = sum(size)
   ) |>
   dplyr::ungroup()

   matrix |>
   e_charts(x) |>
   e_scatter_3d(y, z, color, size) |>
   e_visual_map(
     z,
     # scale to z
     inRange = list(symbolSize = c(1, 30)),
     # scale size
     dimension = 3 # third dimension 0 = x, y = 1, z = 2, size = 3
   ) |>
   e_visual_map(
     z,
     # scale to z
     inRange = list(color = c("#bf444c", "#d88273", "#f6efa6")),
     # scale colors
     dimension = 4,
     # third dimension 0 = x, y = 1, z = 2, size = 3, color = 4
     bottom = 300 # padding to avoid visual maps overlap
   )


