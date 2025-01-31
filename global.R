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
library(stringr)

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

#PAMF
poblacion <- read_csv("data/tb_poblacion.csv")
  poblacion_totales <- poblacion %>%
    filter(Sexo == 0, Parametro =="PAMF") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Cve_Presupuestal = as.character(Cve_Presupuestal))
    names(poblacion_totales) <- tolower(names(poblacion_totales))

poblacion_gpoedad <- poblacion %>%
    filter(Sexo != 0, Grupo_edad != "NI", Parametro == "PAMF") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')

poblacion_gpoedad_incid <- poblacion_gpoedad %>%
    mutate(Grupo_edad = case_when(
      Grupo_edad == "00 a 04" ~ "de00_a_04",
      Grupo_edad == "05 a 09" ~ "de05_a_09",
      Grupo_edad == "10 a 14" ~ "de10_a_14",
      Grupo_edad == "15 a 19" ~ "de15_a_19",
      Grupo_edad == "20 a 24" ~ "de20_a_24",
      Grupo_edad %in% c("25 a 29", "30 a 34", "35 a 39", "40 a 44") ~ "de25_a_44",
      Grupo_edad == "45 a 49" ~ "de45_a_49",
      Grupo_edad %in% c("50 a 54", "55 a 59") ~ "de50_a_59",
      Grupo_edad == "60 a 64" ~ "de60_a_64",
      Grupo_edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y mas") ~ "de65ymas",
      TRUE ~ Grupo_edad)) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad) %>%
    summarise(Totales_Poblacion = sum(Totales_Poblacion, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Cve_Presupuestal = as.character(Cve_Presupuestal))
names(poblacion_gpoedad_incid)<- tolower(names(poblacion_gpoedad_incid))

#Poblacion Riesgo de Trabajo

cuums <- read_csv("data/cuums_maestro.csv")

poblacion_rt <- poblacion %>% filter(Parametro == "PAU RT") %>%
                left_join(cuums %>% select(ClavePresupuestal, cve_prei = UnidadInformacionPREI),
                          by = c("Cve_Presupuestal" = "ClavePresupuestal")) %>%
                mutate(cve_prei = case_when(Nombre_Unidad == "Jalisco" ~ "14",
                                            Nombre_Unidad == "Nacional" ~ "99",
                                            TRUE ~ cve_prei))

poblacion_totales_rt <- poblacion_rt %>%
  filter(Sexo == 0) %>%
  group_by(Anio, Cve_Presupuestal, Nombre_Unidad, cve_prei, Nombre_OOAD)%>%
  summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Cve_Presupuestal = as.character(Cve_Presupuestal))
  names(poblacion_totales_rt) <- tolower(names(poblacion_totales_rt))

poblacion_gpoedad_rt <- poblacion_rt %>%
    filter(Sexo != 0, Grupo_edad != "NI") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad, cve_prei)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')


#Datos para gráficos
data_censo <- reactiveVal({
  data <- read_csv("data/tb_censo_DM.csv") %>%
            rename(Dato= Pacientes_DM) #Pacientes_DM -> Numero de pacientes Prevalencia_DM -> prevalencia
  data
  })

data_consulta <- reactiveVal(read_csv("data/tb_consulta_dm.csv"))

data_incapacidad <- reactiveVal({
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
})

data_hosp <- reactiveVal(read_csv("data/tb_egreso_dm.csv"))


data_incidencia <- reactiveVal({
data <- read_csv("data/tb_incidencia_dm.csv") %>%
    filter(grupos != "TTotal" & grupos != "seignora")%>%
    # Group by relevant columns and sum cases
    group_by(cve_presupuestal, anio, cve_delega, cve_diagno, sexo, grupos) %>%
    mutate(grupos = case_when(grupos %in% c("men1", "de01_a_04") ~ "de00_a_04", 
      TRUE ~ grupos)) %>%
    summarise(casos = sum(casos), .groups = "drop") %>%
    # Join with population data
    full_join(poblacion_gpoedad_incid, 
              by = c("cve_presupuestal", "sexo" = "sexo", "anio" = "anio", "grupos" = "grupo_edad")) %>%
    # Calculate incidence rate
    mutate(Dato = casos / totales_poblacion * 100000) %>%
    # Rename columns to match the previous format
    rename(
      Anio = anio,
      Nombre_Unidad = nombre_unidad,
      Sexo = sexo,
      Grupo_edad = grupos
    )
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



## Datos para métricas 
#Prevalencia
totales_anuales <- reactiveVal({
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    data <- read_csv("data/tb_censo_DM.csv")
    data %>% 
      filter (Sexo == 0) %>%
      mutate(Nombre_Unidad = ifelse(nchar(Cve_Presupuestal) < 3 & Cve_Presupuestal != "00", "OOAD", Nombre_Unidad)) %>%
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
    data <- data %>% 
      filter(Parametro == 'Consulta_MF', Sexo == 0, Grupo_edad=="Total") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
      summarise(Dato = sum(Dato, na.rm = TRUE), .groups = 'drop') %>%
      rename(anio = Anio, cve_presupuestal = Cve_Presupuestal) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
      mutate(Dato2 = Dato/totales_poblacion*1000) %>%
      select(Anio = anio,Nombre_OOAD, Nombre_Unidad, Dato, totales_poblacion, Dato2)
})


totales_incap <- reactiveVal({
    data <- read_csv("data/tb_dm_incap.csv")
    data <- data %>% 
      rename(Anio = PERIODO, Nombre_Unidad = descnivel) %>%
      group_by(Anio, Nombre_Unidad, NIVEL) %>%
      summarise(ndias = sum(NDIAS, na.rm = TRUE), 
                Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                .groups = 'drop') %>%
      mutate(NIVEL = as.character(NIVEL)) %>%
      left_join(poblacion_totales_rt %>% select(cve_prei, nombre_ooad, anio, totales_poblacion), 
                by = c('NIVEL' = 'cve_prei', "Anio" = "anio")) %>%
      mutate(Dato = ndias/totales_poblacion*100) %>%
      ##cambiar este case_when para que funcione cuando se agreguen mas estados, se puede hacer
      ##un join con la tabla de estados que se puede extraer del cuums_maestro
      mutate(Nombre_Unidad = case_when(is.na(Nombre_Unidad) ~ NA_character_,
                                       str_detect(Nombre_Unidad, "^\\d{2} ") & !str_detect(Nombre_Unidad, "^99 ") ~ "OOAD",
                                       Nombre_Unidad == "99 Nacional" ~ "Nacional",
                                       TRUE ~ str_trim(str_extract(Nombre_Unidad, "(?<=\\s).*"))))
    data
})

totales_hosp <- reactiveVal({
    tryCatch({
    #data <- load_data("SELECT * FROM tb_egreso_dm")
    data <- read_csv("data/tb_egreso_dm.csv")
    data %>%
      filter(Sexo == 0, Parametro == "Egresos_DM", Especialidad == "Total", Grupo_edad=="Total") %>%
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
  data <- read_csv("data/tb_incidencia_dm.csv")
    #data <- load_data("SELECT * FROM dbo.tb_censo_DM")
  data <- data %>%
    # Filter for total rows (assuming 'TTotal' in grupos represents total)
    filter(grupos == "TTotal") %>%
    # Group by year and cve_presupuestal, sum the cases
    group_by(anio, cve_presupuestal) %>%
    summarise(Pacientes_DM = sum(casos, na.rm = TRUE), .groups = "drop") %>%
    # Join with population totals
    inner_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
    # Calculate incidence rate
    mutate(Dato = Pacientes_DM / totales_poblacion * 100000) %>%
    # Rename columns
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

data_censo_maestro <- reactiveVal(read_csv("data/cuums_maestro.csv"))
citiesmx <- reactiveVal(read_csv("data/citiesmx.csv"))

cat_ind <- read_csv("data/cat_indi_dm.csv", locale = locale(encoding = "latin1"))

data_indicadores <- reactiveVal({
  data <- read_csv("data/tb_datos_historico_indicadores.csv") %>%
    mutate(nombre_unidad = case_when(
      nombre_unidad %in% c("UMF  168 TEPATITLAN", "UMF 168 TEPATITLAN", "UMF 168 Tepatitlán") ~ "UMF 168 Tepatitlán",
      TRUE ~ nombre_unidad
    )) %>% left_join(cat_ind, by=join_by(nom_indicador==codigo)) %>%
    mutate(desc_indicador = paste(nom_indicador, " - ", desc_indicador))
  data
})



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
