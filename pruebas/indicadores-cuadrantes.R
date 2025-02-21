# 1. Load required libraries
library(tidyverse)
library(echarts4r)
library(lubridate)
library(ggrepel)
library(plotly)

# 2. Read the data
cuums <- read_csv("data/cuums_maestro.csv")
cat_ind <- read_csv("data/cat_indi_dm.csv", locale = locale(encoding = "utf-8"))
data <- read_csv("data/tb_datos_historico_indicadores.csv")

# 3. Data preparation (replacing reactiveVal)
data_indicadores <- data %>%
  select(-nombre_unidad) %>%
  filter(digito_equivalencia %in% c(558, 470, 5, 1, 8, 9, 7, 3, 6)) %>%
  left_join(select(cuums, ClavePresupuestal, DenominacionUnidad), 
            by = c("clave" = "ClavePresupuestal")) %>%
  left_join(cat_ind, by = join_by(nom_indicador == codigo)) %>%
  mutate(nombre_unidad = case_when(
    clave == "00" ~ "Nacional",
    clave == "14" ~ "Jalisco",
    TRUE ~ DenominacionUnidad
  )) %>%
  mutate(desc_indicador = paste(nom_indicador, " - ", desc_indicador))

# 4. Set test parameters (replacing Shiny inputs)
selected_year <- 2023  # Replace with your desired year
selected_umf <- "UMF 168 Tepatitlán"  # Replace with your desired UMF
indicator_x <- unique(data_indicadores$desc_indicador)[1]  # First indicator
indicator_y <- unique(data_indicadores$desc_indicador)[2]  # Second indicator

# 5. Prepare correlation data
correlation_data <- data_indicadores %>%
  filter(anio == selected_year, mes_i == 12) %>%
  select(nombre_unidad, desc_indicador, indicador) %>%
  pivot_wider(
    names_from = desc_indicador,
    values_from = indicador,
    names_repair = "unique_quiet"
  ) %>%
  select(nombre_unidad, all_of(c(indicator_x, indicator_y))) %>%
  mutate(across(-nombre_unidad, ~as.numeric(.) %>% round(2)))

# 6. Calculate means for quadrant lines
x_mean <- mean(correlation_data[[indicator_x]], na.rm = TRUE)
y_mean <- mean(correlation_data[[indicator_y]], na.rm = TRUE)

# 7. Create quadrant plot
p <- ggplot(correlation_data, aes(x = .data[[indicator_x]], 
                                 y = .data[[indicator_y]],
                                 text = paste0("Unidad: ", nombre_unidad,
                                             "\n", names(correlation_data)[2], ": ", 
                                             round(.data[[indicator_x]], 2),
                                             "\n", names(correlation_data)[3], ": ", 
                                             round(.data[[indicator_y]], 2)))) +
  # Add quadrant lines
  geom_vline(xintercept = x_mean, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = y_mean, linetype = "dashed", color = "gray") +
  # Add regular points
  geom_point(data = . %>% filter(!nombre_unidad %in% c(selected_umf, "Nacional", "Jalisco")),
             size = 1.5, alpha = 0.6, color = "gray70") +
  # Add highlighted points
  geom_point(data = . %>% filter(nombre_unidad %in% c(selected_umf, "Nacional", "Jalisco")),
             aes(color = nombre_unidad), size = 3) +
  # Add labels for highlighted points
  geom_text_repel(data = . %>% filter(nombre_unidad %in% c(selected_umf, "Nacional", "Jalisco")),
                  aes(label = nombre_unidad, color = nombre_unidad), size = 4, show.legend = FALSE) +
  # Custom colors
  scale_color_manual(values = c("Nacional" = "#E41A1C", 
                               "Jalisco" = "#377EB8", 
                               "UMF 168 Tepatitlán" = "#4DAF4A")) +
  # Add quadrant labels
  annotate("text", x = max(correlation_data[[indicator_x]], na.rm = TRUE), 
           y = max(correlation_data[[indicator_y]], na.rm = TRUE), 
           label = "Alto-Alto", hjust = 1, vjust = 1) +
  annotate("text", x = min(correlation_data[[indicator_x]], na.rm = TRUE), 
           y = max(correlation_data[[indicator_y]], na.rm = TRUE), 
           label = "Bajo-Alto", hjust = 0, vjust = 1) +
  annotate("text", x = max(correlation_data[[indicator_x]], na.rm = TRUE), 
           y = min(correlation_data[[indicator_y]], na.rm = TRUE), 
           label = "Alto-Bajo", hjust = 1, vjust = 0) +
  annotate("text", x = min(correlation_data[[indicator_x]], na.rm = TRUE), 
           y = min(correlation_data[[indicator_y]], na.rm = TRUE), 
           label = "Bajo-Bajo", hjust = 0, vjust = 0) +
  # Customize theme
  theme_minimal() +
  labs(
    title = paste("Análisis de cuadrante:", selected_year),
    x = indicator_x,
    y = indicator_y,
    color = "Unidad"
  )

# Convert to interactive plot
ggplotly(p, tooltip = "text")
