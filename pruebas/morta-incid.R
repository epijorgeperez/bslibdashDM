library(dplyr)
library(readr)
library(tidyr)
#source("pruebas/conexion.R")
#totales_incidencia <- reactiveVal({
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    #poblacion <- read_csv("data/tb_poblacion.csv")

pob <- load_data("SELECT * FROM dbo.tb_poblacion")
    poblacion_totales <- pob %>%
    filter(Sexo == 0) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')%>%
    mutate(Cve_Presupuestal = as.character(Cve_Presupuestal))
    names(poblacion_totales) <- tolower(names(poblacion_totales))

poblacion_gpoedad <- pob %>%
    filter(Sexo != 0, Grupo_edad != "NI", Parametro == "PAMF") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')

# Totales incidencia

#totales_incidencia <- reactiveVal({
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    data_i <- read_csv("data/tb_incidencia_dm.csv")
    data_i %>%
      filter(Grupo_edad == "TTotal", Sexo == 0) %>%
      rename(anio=Anio, cve_presupuestal= Cve_Presupuestal) %>%
      group_by(anio, cve_presupuestal) %>%
    summarise(Pacientes_DM = sum(Dato, na.rm = TRUE), .groups = "drop") %>%
    inner_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
    mutate(Dato = Pacientes_DM / totales_poblacion * 100000) %>%
    rename(Nombre_Unidad = nombre_unidad, Anio = anio)

# Incidencia por sexo y edad

poblacion_gpoedad_incid <- poblacion_gpoedad %>%
    mutate(Grupo_edad = case_when(
            Grupo_edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y mas") ~ "65 y mas",
            Grupo_edad %in% c("25 a 29", "30 a 34", "35 a 39", "40 a 44") ~ "25 a 44",
            Grupo_edad %in% c("50 a 54", "55 a 59") ~ "50 a 59",
            TRUE ~ Grupo_edad)) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad) %>%
    summarise(Totales_Poblacion = sum(Totales_Poblacion, na.rm = TRUE), .groups = 'drop')
names(poblacion_gpoedad_incid)<- tolower(names(poblacion_gpoedad_incid))

incid_sexo_edad <- data_i  %>%
    filter(Sexo != 0, !Grupo_edad %in% c("TTotal", "seignora")) %>%
    mutate(Cve_Presupuestal = case_when(
      Nombre_OOAD == "Nacional" ~ "00",
      Nombre_OOAD == "Jalisco" & is.na(Cve_Presupuestal) ~ "14",
      TRUE ~ Cve_Presupuestal
    )) %>%
    mutate(Grupo_edad = case_when(
      Grupo_edad %in% c("men1", "de01_a_04") ~ "00 a 04",
      TRUE ~ Grupo_edad
    )) %>%
    group_by(Cve_Presupuestal, Anio, Sexo, Grupo_edad) %>%
    summarise(Casos = sum(Dato), .groups = 'drop')%>%
    mutate(Grupo_edad = ifelse(grepl("de65", Grupo_edad), "65 y mas", Grupo_edad)) %>%
    mutate(Grupo_edad = gsub("de(\\d+)_a_(\\d+)", "\\1 a \\2", Grupo_edad)) %>%
    rename_with(tolower)%>%
    full_join(poblacion_gpoedad_incid, by = c("cve_presupuestal", "sexo", "anio", "grupo_edad")) %>%
    mutate(Dato = casos/totales_poblacion*100000)%>%
    rename(Anio = anio, Nombre_Unidad = nombre_unidad, Sexo = sexo, Grupo_edad = grupo_edad)


filtered  <- incid_sexo_edad  %>% 
        filter(Anio == 2023,  Nombre_Unidad == "UMF 34 Guadalajara", Sexo %in% c(1, 2), Grupo_edad != "Total") #Nombre_OOAD == ooad(), agregar filtro de ooad si es necesario
       
      # To stack by sex, ensure 'Sexo' is a factor with the desired level names
      filtered$Sexo <- factor(filtered$Sexo, levels = c(1, 2), labels = c("Hombre", "Mujer"))
  
      # Group and summarise data
      data_grouped <- filtered %>%
        group_by(Grupo_edad, Sexo) %>%
        summarise(Casos = sum(casos, na.rm = TRUE), .groups = 'drop')

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



# Totales mortalidad

poblacion_totales <- pob %>%
  filter(Sexo == 0, Parametro =="PAMF") %>%
  group_by(Anio, Cve_Presupuestal, Nombre_Unidad)%>%
  summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Cve_Presupuestal = as.character(Cve_Presupuestal),
         Anio = as.character(Anio))  # Ensure Anio is character
  names(poblacion_totales) <- tolower(names(poblacion_totales))

totales <- mortalidad %>% 
    filter(Grupo_edad == "Total", Sexo == 0) %>%
    mutate(Cve_Presupuestal = case_when(
    Nombre_OOAD == "Nacional" ~ "00",
    Nombre_OOAD == "Jalisco" & is.na(Nombre_Unidad) ~ "14",
    TRUE ~ Cve_Presupuestal
  )) %>%
    rename(anio=Anio, cve_presupuestal= Cve_Presupuestal) %>%
    mutate(anio = as.character(anio),
           cve_presupuestal = as.character(cve_presupuestal)) %>%
    group_by(anio, cve_presupuestal) %>% 
    summarise(count = sum(Dato), .groups = 'drop') %>% #filter(Cve_Presupuestal=="00")
    left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
    mutate(Dato = count/totales_poblacion*100000) %>%
    rename(Nombre_Unidad = nombre_unidad, Anio = anio)

# Mortalidad por edad y sexo
poblacion_gpoedad <- poblacion %>%
    filter(Sexo != 0, Grupo_edad != "NI") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')

data_m <- mort %>%
    filter(Sexo != 0, Grupo_edad != "Total") %>%
    group_by(Cve_Presupuestal, Sexo, Anio, Grupo_edad) %>%
    summarise(Casos = sum(Dato), .groups = 'drop')%>%
    full_join(poblacion_gpoedad, by = c("Cve_Presupuestal", "Sexo", "Anio", "Grupo_edad")) %>%
    mutate(Casos = ifelse(is.na(Casos), 0, Casos)) %>%
    mutate(Dato = Casos/Totales_Poblacion * 100000) 




#Incidencia

incid %>%
    filter(Grupo_edad == "TTotal", Sexo == 0) %>%
    mutate(Cve_Presupuestal = case_when(
    Nombre_OOAD == "Nacional" ~ "00",
    startsWith(Nombre_OOAD, "Jalisco") & Nombre_Unidad == "" ~ "14",
    TRUE ~ Cve_Presupuestal
  )) %>%
    rename(anio=Anio, cve_presupuestal= Cve_Presupuestal) %>%
    mutate(anio = as.character(anio),
           cve_presupuestal = as.character(cve_presupuestal)) %>% 
    group_by(anio, cve_presupuestal) %>%
    summarise(Pacientes_DM = sum(Dato, na.rm = TRUE), .groups = "drop") %>% 
    inner_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
    mutate(Dato = Pacientes_DM / totales_poblacion * 100000) %>%
    rename(Nombre_Unidad = nombre_unidad, Anio = anio)




incid %>% 
  filter(Nombre_OOAD == "Jalisco") %>%
  select(Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
  distinct()
incid %>% 
  filter(grepl("^Jalisco\\s*", Nombre_OOAD) & Nombre_Unidad == "") %>%
  select(Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
  distinct()


tail(incid)

unique(incid$Nombre_OOAD)


## RANDOM
      data %>%
        filter(anio == 2014) %>%
        group_by(grupo_edad, sexo) %>%
        summarise(Casos = sum(Dato, na.rm = TRUE), .groups = 'drop')

pob %>% left_join(cuums %>% select(ClavePresupuestal, UnidadInformacionPREI),
                by = c("Cve_Presupuestal" = "ClavePresupuestal"))
