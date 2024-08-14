library(dplyr)
library(readr)
library(tidyr)

#totales_incidencia <- reactiveVal({
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    poblacion <- read_csv("data/tb_poblacion.csv")
    poblacion_totales <- poblacion %>%
    filter(Sexo == 0) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')%>%
    mutate(Cve_Presupuestal = as.character(Cve_Presupuestal))
    names(poblacion_totales) <- tolower(names(poblacion_totales))

# Totales incidencia

#totales_incidencia <- reactiveVal({
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    data <- read_csv("data/tb_incidencia_dm.csv")
    data <- data %>% 
      group_by(anio, cve_presupuestal) %>%
      summarise(Pacientes_DM = sum(no_totales, na.rm = TRUE)) %>%
      inner_join(poblacion_totales, by = c("anio", "cve_presupuestal"))%>%
                mutate(Dato = Pacientes_DM/totales_poblacion*100000) %>%
      rename(Nombre_Unidad = nombre_unidad, Anio = anio)
    data
#})


incid_nac <- reactive({
  # Accessing the reactive 'totales_hosp' from the server environment
  filtered_in_nac <- totales_incidencia() %>%
    filter(
      Anio == anio(),
      #Nombre_OOAD == ooad(),
      #Nombre_Unidad == unidad_medica()
    )
  filtered_in_nac
})



    data <- read_csv("data/tb_incidencia_dm.csv")
    data <- data %>% 
      group_by(anio, cve_presupuestal) %>%
      summarise(Pacientes_DM = sum(no_totales, na.rm = TRUE)) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal"))%>%
                mutate(Dato = Pacientes_DM/totales_poblacion*100000)
#})






poblacion <- read_csv("data/tb_poblacion.csv")
poblacion_totales <- poblacion %>%
    filter(Sexo == 0) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')
    names(poblacion_totales) <- tolower(names(poblacion_totales))



#totales_incidencia <- reactiveVal({
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    poblacion <- read_csv("data/tb_poblacion.csv")
    poblacion_totales <- poblacion %>%
    filter(Sexo == 0) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')
    names(poblacion_totales) <- tolower(names(poblacion_totales))

    data <- read_csv("data/tb_incidencia_dm.csv")
    data <- data %>% 
      group_by(anio, cve_presupuestal) %>%
      summarise(Pacientes_DM = sum(no_totales, na.rm = TRUE)) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal"))%>%
                mutate(Dato = Pacientes_DM/totales_poblacion*100000)
#})


# Totales mortalidad

mort<- read_csv("data/tb_morta_dm.csv")

data <- mort %>% 
      group_by(anio_def, uni_adsc) %>%
      summarise(count = n(), .groups = 'drop') %>%
      rename(anio = anio_def, cve_presupuestal = uni_adsc) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal"))%>%
                mutate(Dato = count/totales_poblacion*100000)


data_m <- read_csv("data/tb_morta_DM.csv") %>%
    group_by(uni_adsc, cvesex, anio_def, gpoedad) %>%
    summarise(Casos = n(), .groups = 'drop')%>%
    mutate(gpoedad = sub("-", " a ", gpoedad)) %>%
    mutate(gpoedad = ifelse(gpoedad == "85 y +", "85 y mas", gpoedad))%>%
    mutate(gpoedad = ifelse(nchar(gpoedad) == 6, paste0("0", gpoedad), gpoedad))%>%
    rename(Anio = anio_def, Sexo = cvesex, Grupo_edad = gpoedad, Cve_Presupuestal = uni_adsc)%>%
    full_join(poblacion_gpoedad, by = c("Cve_Presupuestal", "Sexo", "Anio", "Grupo_edad")) %>%
    mutate(Casos = ifelse(is.na(Casos), 0, Casos)) %>%
    mutate(Dato = Casos/Totales_Poblacion * 100000) 

poblacion_gpoedad <- poblacion %>%
    filter(Sexo != 0, Grupo_edad != "NI") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')

# Totales incidencia

poblacion_gpoedad_incid <- poblacion_gpoedad %>%
    mutate(Grupo_edad = case_when(
            Grupo_edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y mas") ~ "65 y mas",
            Grupo_edad %in% c("25 a 29", "30 a 34", "35 a 39", "40 a 44") ~ "25 a 44",
            Grupo_edad %in% c("50 a 54", "55 a 59") ~ "50 a 59",
            TRUE ~ Grupo_edad)) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad) %>%
    summarise(Totales_Poblacion = sum(Totales_Poblacion, na.rm = TRUE), .groups = 'drop')
names(poblacion_gpoedad_incid)<- tolower(names(poblacion_gpoedad_incid))

data <- read_csv("data/tb_incidencia_dm.csv") %>%
    select(c(1,2,12:36))%>%
    group_by(cve_presupuestal, anio) %>%
    summarise_all(sum)%>%
    mutate(de00_a_04 = menores_1 + de01_a_04, 
            de00_a_04f = menores_1f + de01_a_04f)%>%
    select(1,2,28,3:14, 29, 15:27) %>%
    select(-c("menores_1", "menores_1f", "de01_a_04", "de01_a_04f", "se_ignoran", "se_ignoraf", "no_totales")) %>%
    pivot_longer(cols = 3:last_col(), names_to = "gpoedadsexo", values_to = "casos")%>%
    mutate(sexo = ifelse(grepl("f$", gpoedadsexo), 2, 1)) %>%
    mutate(grupo_edad = sub("de([0-9]+)_a_([0-9]+)", "\\1 a \\2", gpoedadsexo))%>%
    mutate(grupo_edad = sub("f$", "", grupo_edad))%>%
    mutate(grupo_edad = ifelse(grepl("de65", grupo_edad), "65 y mas", grupo_edad)) %>%
    select(1,2,6,5,4)%>%
    full_join(poblacion_gpoedad_incid, by = c("cve_presupuestal", "sexo", "anio", "grupo_edad")) %>%
    mutate(Dato = casos/totales_poblacion*100000) %>%
    rename(Anio = anio, Nombre_Unidad = nombre_unidad, Sexo = sexo, Grupo_edad = grupo_edad)

      data %>%
        filter(anio == 2014) %>%
        group_by(grupo_edad, sexo) %>%
        summarise(Casos = sum(Dato, na.rm = TRUE), .groups = 'drop')

