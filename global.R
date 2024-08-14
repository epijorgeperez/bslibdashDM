# Global variables and library loading can go here
library(shiny)
library(bslib)
library(DBI)
library(odbc)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(dygraphs)
library(lubridate)
library(echarts4r)
library(leaflet)
library(xts)
library(gt)

# Connection string to establish a connection to the SQL Server database
#connection_details <- dbConnect(odbc::odbc(), 
#                                Driver = "SQL Server", 
#                                Server = "MTO02438WSCHG7", 
#                                Database = "DAS_DM", 
#                                Trusted_Connection = "Yes")

# Function to load data from the database
#load_data <- function(query) {
#  data <- dbGetQuery(connection_details, query)
#  return(data)
#}

# Example of loading specific tables
 #censo <- load_data("SELECT * FROM dbo.tb_censo_DM")
 #consultas <- load_data("SELECT * FROM tb_consulta_dm")
 #indicadores <- load_data("SELECT * FROM dbo.tb_datos_historico_indicadores")
 #incapacidades <- load_data("SELECT * FROM dbo.tb_dm_incap")
 #poblacion <- load_data("SELECT * FROM dbo.tb_poblacion")

# global.R
metric_choices <- c("Incidencia", "Prevalencia", "Consultas", "Hospitalizaciones", "Mortalidad", "Incapacidades")

poblacion <- read_csv("data/tb_poblacion.csv")
  poblacion_totales <- poblacion %>%
    filter(Sexo == 0) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Cve_Presupuestal = as.character(Cve_Presupuestal))
    names(poblacion_totales) <- tolower(names(poblacion_totales))

poblacion_gpoedad <- poblacion %>%
    filter(Sexo != 0, Grupo_edad != "NI") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')

poblacion_gpoedad_incid <- poblacion_gpoedad %>%
    mutate(Grupo_edad = case_when(
            Grupo_edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y mas") ~ "65 y mas",
            Grupo_edad %in% c("25 a 29", "30 a 34", "35 a 39", "40 a 44") ~ "25 a 44",
            Grupo_edad %in% c("50 a 54", "55 a 59") ~ "50 a 59",
            TRUE ~ Grupo_edad)) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad) %>%
    summarise(Totales_Poblacion = sum(Totales_Poblacion, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Cve_Presupuestal = as.character(Cve_Presupuestal))
names(poblacion_gpoedad_incid)<- tolower(names(poblacion_gpoedad_incid))

data_censo <- reactiveVal({
  data <- read_csv("data/tb_censo_DM.csv") %>%
            rename(Dato= Pacientes_DM) #Pacientes_DM -> Numero de pacientes Prevalencia_DM -> prevalencia
  data
  })

data_consulta <- reactiveVal(read_csv("data/tb_consulta_dm.csv"))

data_incapacidad <- reactiveVal({
  data <- read_csv("data/tb_dm_incap.csv") %>%
    rename(Anio = PERIODO, Nombre_Unidad = descnivel, Grupo_edad = descgedad, Sexo = TIP_SEXO)%>%
    rename(Dato = NDIAS)
  data
})

data_hosp <- reactiveVal(read_csv("data/tb_egreso_dm.csv"))


data_incidencia <- reactiveVal({
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
    mutate(Dato = casos/totales_poblacion*100000)%>%
    rename(Anio = anio, Nombre_Unidad = nombre_unidad, Sexo = sexo, Grupo_edad = grupo_edad)

data
  
})


data_mortalidad <- reactiveVal({
#SELECT uni_adsc, cvesex, anio_def, gpoedad, COUNT(*) AS Dato
#FROM MORTA_UNIDAD 
#GROUP BY uni_adsc, cvesex, anio_def, gpoedad;
data <- read_csv("data/tb_morta_dm.csv") %>%
    group_by(uni_adsc, cvesex, anio_def, gpoedad) %>%
    summarise(Casos = n(), .groups = 'drop')%>%
    mutate(gpoedad = sub("-", " a ", gpoedad)) %>%
    mutate(gpoedad = ifelse(gpoedad == "85 y +", "85 y mas", gpoedad))%>%
    mutate(gpoedad = ifelse(nchar(gpoedad) == 6, paste0("0", gpoedad), gpoedad))%>%
    rename(Anio = anio_def, Sexo = cvesex, Grupo_edad = gpoedad, Cve_Presupuestal = uni_adsc)%>%
    full_join(poblacion_gpoedad, by = c("Cve_Presupuestal", "Sexo", "Anio", "Grupo_edad")) %>%
    mutate(Casos = ifelse(is.na(Casos), 0, Casos)) %>%
    mutate(Dato = Casos/Totales_Poblacion * 100000) 
data
})




#Prevalencia
totales_anuales <- reactiveVal({
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    data <- read_csv("data/tb_censo_DM.csv")
    data %>% 
      filter (Sexo == 0) %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
      summarise(Pacientes_DM = sum(Pacientes_DM, na.rm = TRUE), 
                PAMF = sum(PAMF, na.rm = TRUE),
                Dato = Pacientes_DM / PAMF * 100,
                .groups = 'drop')
})

#Consultas
totales_consultas <- reactiveVal({

    #data <- load_data("SELECT * FROM tb_consulta_DM")
    data <- read_csv("data/tb_consulta_dm.csv")
    data %>% 
      filter (Parametro == 'Consulta_MF', Sexo == 0) %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
      summarise(Dato = sum(Dato, na.rm = TRUE), .groups = 'drop') %>%
      rename(anio = Anio, cve_presupuestal = Cve_Presupuestal) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
      mutate(Dato2 = Dato/totales_poblacion*1000) %>%
      select(Anio = anio,Nombre_OOAD, Nombre_Unidad, Dato, totales_poblacion, Dato2)
})

totales_incap <- reactiveVal({
    tryCatch({
    #data <- load_data("SELECT * FROM tb_dm_incap")
    data <- read_csv("data/tb_dm_incap.csv")
    data %>% 
      group_by(Anio = PERIODO, Nombre_Unidad = descnivel) %>%
      summarise(Dato = sum(NDIAS, na.rm = TRUE), 
                Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                .groups = 'drop')
  }, error = function(e) {
    # Handle any errors that occur when loading the data
    showNotification(paste("Error loading data:", e$message), type = "error")
    return(NULL)
  })
})

totales_hosp <- reactiveVal({
    tryCatch({
    #data <- load_data("SELECT * FROM tb_egreso_dm")
    data <- read_csv("data/tb_egreso_dm.csv")
    data %>%
      filter(Sexo == 0, Parametro == "Egresos_DM") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
      summarise(Dato = sum(Dato, na.rm = TRUE),.groups = 'drop') %>%
      rename(anio = Anio, cve_presupuestal = Cve_Presupuestal) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
      mutate(Dato2 = Dato/totales_poblacion*100000) %>%
      select(Anio = anio,Nombre_OOAD, Nombre_Unidad, Dato, totales_poblacion, Dato2)
  }, error = function(e) {
    # Handle any errors that occur when loading the data
    showNotification(paste("Error loading data:", e$message), type = "error")
    return(NULL)
  })
})

totales_dias_estancia<- reactiveVal({
    tryCatch({
    #data <- load_data("SELECT * FROM tb_egreso_dm")
    data <- read_csv("data/tb_egreso_dm.csv")
    data %>% 
      filter(Sexo == 0, Parametro == "DiasEstancia_DM") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
      summarise(Dato = mean(Dato, na.rm = TRUE),
                .groups = 'drop')
  }, error = function(e) {
    # Handle any errors that occur when loading the data
    showNotification(paste("Error loading data:", e$message), type = "error")
    return(NULL)
  })
})

totales_incidencia <- reactiveVal({
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    data <- read_csv("data/tb_incidencia_dm.csv")
    data <- data %>% 
      group_by(anio, cve_presupuestal) %>%
      summarise(Pacientes_DM = sum(no_totales, na.rm = TRUE)) %>%
      #left_join(poblacion_totales, by = c("anio", "cve_presupuestal"))%>% cambie a inner join porque solo tengo población de Tepatitlán.
      inner_join(poblacion_totales, by = c("anio", "cve_presupuestal"))%>%
                mutate(Dato = Pacientes_DM/totales_poblacion*100000) %>%
      rename(Nombre_Unidad = nombre_unidad, Anio = anio)
    data
})

totales_mortalidad <- reactiveVal({
    mort<- read_csv("data/tb_morta_dm.csv")

    data <- mort %>% 
          group_by(anio_def, uni_adsc) %>%
          summarise(count = n(), .groups = 'drop') %>%
          rename(anio = anio_def, cve_presupuestal = uni_adsc) %>%
          mutate(cve_presupuestal= as.character(cve_presupuestal))%>%
          left_join(poblacion_totales, by = c("anio", "cve_presupuestal"))%>%
                    mutate(Dato = count/totales_poblacion*100000)%>%
          rename(Nombre_Unidad = nombre_unidad, Anio = anio)
    data                
})

data_censo_maestro <- reactiveVal(read_csv("data/CUMMS_MAESTRO.csv"))

citiesmx <- reactiveVal(read_csv("data/citiesmx.csv"))

data_indicadores <- reactiveVal(read_csv("data/tb_datos_historico_indicadores.csv"))





# Connection string to establish a connection to the SQL Server database
#connection_details <- dbConnect(odbc::odbc(), 
#                                Driver = "SQL Server", 
#                                Server = "MTO02438WSCHG7", 
#                                Database = "DAS_DM", 
#                                Trusted_Connection = "Yes")

# Function to load data from the database
#load_data <- function(query) {
#  data <- dbGetQuery(connection_details, query)
#  return(data)
#}

# Example of loading specific tables
 #censo <- load_data("SELECT * FROM dbo.tb_censo_DM")
 #consultas <- load_data("SELECT * FROM tb_consulta_dm")
 #indicadores <- load_data("SELECT * FROM dbo.tb_datos_historico_indicadores")
 #incapacidades <- load_data("SELECT * FROM dbo.tb_dm_incap")
 #poblacion <- load_data("SELECT * FROM dbo.tb_poblacion")
