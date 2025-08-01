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
library(leaflet.extras)
library(sf)
library(plotly)
library(ggrepel)
library(dotenv)

# Configure Shiny encoding options for proper Spanish character display
options(
  shiny.encoding = "UTF-8",
  readr.default_locale = readr::locale(encoding = "UTF-8")
)

# Configurar variables de entorno para usuario shiny
Sys.setenv(OPENSSL_CONF = "/etc/shiny-server/openssl.cnf")

# Cargar variables de entorno desde .env
if (file.exists(".env")) {
  readRenviron(".env")
  message("✅ Variables de entorno cargadas desde .env")
} else {
  message("⚠️ Archivo .env no encontrado")
}

# Verificar variables críticas
message("🔧 OPENSSL_CONF: ", Sys.getenv("OPENSSL_CONF"))
message("👤 DB_USER: ", Sys.getenv("DB_USER"))


# Initialize connection_details to NULL before attempting connection
connection_details <- NULL

# Configuración fija
DB_SERVER <- "11.33.41.96"
DB_NAME <- "DAS_DM"

# Credenciales desde variables de entorno
DB_USER <- Sys.getenv("DB_USER", "")
DB_PASSWORD <- Sys.getenv("DB_PASSWORD", "")

# Validar credenciales
if (DB_USER == "" || DB_PASSWORD == "") {
  stop("❌ DB_USER and DB_PASSWORD environment variables are required")
}

# Función de conexión simplificada
get_database_connection <- function() {
  message("🔌 Connecting to SQL Server...")
  
  tryCatch({
    connection <- dbConnect(
      odbc::odbc(),
      Driver = "ODBC Driver 17 for SQL Server",
      Server = DB_SERVER,
      Database = DB_NAME,
      UID = DB_USER,
      PWD = DB_PASSWORD,
      encoding = "UTF-8"
    )
    
    # Test connection
    test_result <- dbGetQuery(connection, "SELECT 1 AS test")
    if (nrow(test_result) == 1) {
      message("✅ Database connected successfully")
      return(connection)
    } else {
      stop("Connection test failed")
    }
    
  }, error = function(e) {
    message("❌ Connection failed: ", e$message)
    stop("Database connection is required for this application")
  })
}

# Establecer conexión global
message("=== Starting Database Connection ===")
connection_details <- get_database_connection()

# Función para cargar datos (simplificada)
load_data <- function(query) {
  if (!dbIsValid(connection_details)) {
    stop("No active database connection")
  }
  
  tryCatch({
    data <- dbGetQuery(connection_details, query)
    message("✅ Query OK. Rows: ", nrow(data))
    
    # Fix encoding
    data[] <- lapply(data, function(x) {
      if (is.character(x)) {
        Encoding(x) <- "UTF-8"
      }
      return(x)
    })
    
    return(data)
  }, error = function(e) {
    message("❌ Query failed: ", e$message)
    stop("Database query failed: ", e$message)
  })
}

# Cerrar conexión al terminar
onStop(function() {
  if (exists("connection_details") && dbIsValid(connection_details)) {
    dbDisconnect(connection_details)
    message("🔌 Database disconnected")
  }
})

message("=== Database setup complete ===")


# global.R
metric_choices <- c("Incidencia", "Prevalencia", "Consultas", "Hospitalizaciones", "Mortalidad", "Incapacidades")

#PAMF
# Load population data from SQL Server with CSV fallback
poblacion <- load_data("SELECT * FROM dbo.tb_poblacion")
message(head(poblacion,1))


poblacion_totales <- poblacion %>%
  filter(Sexo == 0, Parametro =="PAMF") %>%
  group_by(Anio, Cve_Presupuestal, Nombre_Unidad)%>%
  summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Cve_Presupuestal = as.character(Cve_Presupuestal),
         Anio = as.character(Anio))  # Ensure Anio is character
  names(poblacion_totales) <- tolower(names(poblacion_totales))

poblacion_gpoedad <- poblacion %>%
    filter(Sexo != 0, Grupo_edad != "NI", Parametro == "PAMF") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')

poblacion_gpoedad_incid <- poblacion_gpoedad %>%
    mutate(Grupo_edad = case_when(
            Grupo_edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y mas") ~ "65 y mas",
            Grupo_edad %in% c("25 a 29", "30 a 34", "35 a 39", "40 a 44") ~ "25 a 44",
            Grupo_edad %in% c("50 a 54", "55 a 59") ~ "50 a 59",
            TRUE ~ Grupo_edad)) %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad) %>%
    summarise(Totales_Poblacion = sum(Totales_Poblacion, na.rm = TRUE), .groups = 'drop')
names(poblacion_gpoedad_incid)<- tolower(names(poblacion_gpoedad_incid))

#Poblacion Riesgo de Trabajo
# Load CUUMS master data from SQL Server with CSV fallback
cuums <- load_data("SELECT * FROM CUUMS_MAESTRO")


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
  mutate(Cve_Presupuestal = as.character(Cve_Presupuestal),
         Anio = as.character(Anio))  # Ensure Anio is character
  names(poblacion_totales_rt) <- tolower(names(poblacion_totales_rt))

poblacion_gpoedad_rt <- poblacion_rt %>%
    filter(Sexo != 0, Grupo_edad != "NI") %>%
    group_by(Anio, Cve_Presupuestal, Nombre_Unidad, Sexo, Grupo_edad, cve_prei)%>%
    summarise(Totales_Poblacion = sum(Poblacion, na.rm = TRUE), .groups = 'drop')


# ===== OPTIMIZED: Load raw data once =====
message("Loading raw datasets (optimized)...")

# Load each table only once
raw_tb_censo_dm <- load_data("SELECT * FROM dbo.tb_censo_DM")
raw_tb_consulta_dm <- load_data("SELECT * FROM tb_consulta_dm") 
raw_tb_egreso_dm <- load_data("SELECT * FROM dbo.tb_egreso_dm")
raw_tb_dm_incap <- load_data("SELECT * FROM dbo.tb_dm_incap") %>%
  mutate(NDIAS = as.numeric(NDIAS),
         NDIAS = ifelse(is.na(NDIAS), 0, NDIAS),
         FREC = as.numeric(FREC),
         FREC = ifelse(is.na(FREC), 0, FREC))
raw_morbi_diabetes <- load_data("SELECT * FROM MORBI_DIABETES")
raw_morta_diabetes <- load_data("SELECT * FROM dbo.MORTA_DIABETES")

message("Raw datasets loaded successfully")

# ===== DERIVED DATASETS (using raw data) =====

#Datos para gráficos
data_censo <- reactiveVal({
  raw_tb_censo_dm %>% rename(Dato = Pacientes_DM)
})

data_consulta <- reactiveVal(raw_tb_consulta_dm)

data_incapacidad <- reactiveVal({
  tryCatch({
    raw_tb_dm_incap %>%
      rename(Anio = PERIODO, Nombre_Unidad = descnivel, Grupo_edad = descgedad, Sexo = TIP_SEXO) %>%
      group_by(Anio, Nombre_Unidad, NIVEL, Grupo_edad, Sexo) %>%
      summarise(Dato = sum(NDIAS, na.rm = TRUE), 
                Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                .groups = 'drop') %>%
      mutate(Nombre_Unidad = case_when(is.na(Nombre_Unidad) ~ NA_character_,
            Nombre_Unidad == "14 Jalisco" ~ "Jalisco",
            Nombre_Unidad == "99 Nacional" ~ "Nacional",
            TRUE ~ str_trim(str_extract(Nombre_Unidad, "(?<=\\s).*"))))
  }, error = function(e) {
    message("Error processing incapacidad data: ", e$message)
    return(NULL)
  })
})

data_hosp <- reactiveVal(raw_tb_egreso_dm)

data_incidencia <- reactiveVal({
  raw_morbi_diabetes %>%
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
    summarise(Casos = sum(Dato), .groups = 'drop') %>%
    mutate(Grupo_edad = ifelse(grepl("de65", Grupo_edad), "65 y mas", Grupo_edad)) %>%
    mutate(Grupo_edad = gsub("de(\\d+)_a_(\\d+)", "\\1 a \\2", Grupo_edad)) %>%
    rename_with(tolower) %>%
    full_join(poblacion_gpoedad_incid, by = c("cve_presupuestal", "sexo", "anio", "grupo_edad")) %>%
    mutate(Dato = casos/totales_poblacion*100000) %>%
    rename(Anio = anio, Nombre_Unidad = nombre_unidad, Sexo = sexo, Grupo_edad = grupo_edad)
})

data_mortalidad <- reactiveVal({
  raw_morta_diabetes %>%
    filter(Sexo != 0, Grupo_edad != "Total") %>%
    mutate(Cve_Presupuestal = case_when(
      Nombre_OOAD == "Nacional" ~ "00",
      Nombre_OOAD == "Jalisco" & is.na(Cve_Presupuestal) ~ "14",
      TRUE ~ Cve_Presupuestal
    )) %>%
    group_by(Cve_Presupuestal, Sexo, Anio, Grupo_edad) %>%
    summarise(Casos = sum(Dato), .groups = 'drop') %>%
    full_join(poblacion_gpoedad, by = c("Cve_Presupuestal", "Sexo", "Anio", "Grupo_edad")) %>%
    mutate(Casos = ifelse(is.na(Casos), 0, Casos)) %>%
    mutate(Dato = Casos/Totales_Poblacion * 100000)
})

## Datos para métricas (using raw data)
totales_anuales <- reactiveVal({
  raw_tb_censo_dm %>% 
    filter(Sexo == 0) %>%
    mutate(Nombre_Unidad = ifelse(nchar(Cve_Presupuestal) < 3 & Cve_Presupuestal != "00", "OOAD", Nombre_Unidad)) %>%
    group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
    summarise(Pacientes_DM = sum(Pacientes_DM, na.rm = TRUE), 
              PAMF = sum(PAMF, na.rm = TRUE),
              Dato = Pacientes_DM / PAMF * 100,
              .groups = 'drop') %>%
    filter(PAMF != 0)
})

totales_consultas <- reactiveVal({
  raw_tb_consulta_dm %>% 
    filter(Parametro == 'Consulta_MF', Sexo == 0, Grupo_edad=="Total") %>%
    group_by(Anio, Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
    summarise(Dato = sum(Dato, na.rm = TRUE), .groups = 'drop') %>%
    rename(anio = Anio, cve_presupuestal = Cve_Presupuestal) %>%
    mutate(anio = as.character(anio),
           cve_presupuestal = as.character(cve_presupuestal)) %>%
    left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
    mutate(Dato2 = Dato/totales_poblacion*1000) %>%
    rename('Anio' = 'anio', 'Totales_Poblacion' = 'totales_poblacion') %>%
    select(Anio,Nombre_OOAD, Nombre_Unidad, Dato, Totales_Poblacion, Dato2)
})

totales_incap <- reactiveVal({
  tryCatch({
    raw_tb_dm_incap %>% 
      rename(Anio = PERIODO, Nombre_Unidad = descnivel) %>%
      mutate(Anio = as.character(Anio)) %>%
      group_by(Anio, Nombre_Unidad, NIVEL) %>%
      summarise(ndias = sum(NDIAS, na.rm = TRUE), 
                Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                .groups = 'drop') %>%
      mutate(NIVEL = as.character(NIVEL)) %>%
      left_join(
        poblacion_totales_rt %>% 
          select(cve_prei, nombre_ooad, anio, totales_poblacion) %>%
          mutate(anio = as.character(anio)), 
        by = c('NIVEL' = 'cve_prei', "Anio" = "anio")
      ) %>%
      mutate(Dato = ndias/totales_poblacion*100) %>%
      mutate(Nombre_Unidad = case_when(is.na(Nombre_Unidad) ~ NA_character_,
                                      str_detect(Nombre_Unidad, "^\\d{2} ") & !str_detect(Nombre_Unidad, "^99 ") ~ "OOAD",
                                      Nombre_Unidad == "99 Nacional" ~ "Nacional",
                                      TRUE ~ str_trim(str_extract(Nombre_Unidad, "(?<=\\s).*"))))
  }, error = function(e) {
    message("Error processing incapacidad totals: ", e$message)
    return(NULL)
  })
})

totales_hosp <- reactiveVal({
  tryCatch({
    raw_tb_egreso_dm %>%
      filter(Sexo == 0, Parametro == "Egresos_DM_Adsc", Especialidad == "Total", Grupo_edad=="Total") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
      summarise(Dato = sum(Dato, na.rm = TRUE),.groups = 'drop') %>%
      rename(anio = Anio, cve_presupuestal = Cve_Presupuestal) %>%
      mutate(anio = as.character(anio),
             cve_presupuestal = as.character(cve_presupuestal)) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
      mutate(Dato2 = Dato/totales_poblacion*100000) %>%
      select(Anio = anio,Nombre_OOAD, Nombre_Unidad, Dato, totales_poblacion, Dato2)
  }, error = function(e) {
    message("Error processing hospitalization totals: ", e$message)
    return(NULL)
  })
})

totales_dias_estancia <- reactiveVal({
  tryCatch({
    raw_tb_egreso_dm %>% 
      filter(Sexo == 0, Parametro == "DiasEstancia_DM_Adsc") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
      summarise(Dato = mean(Dato, na.rm = TRUE), .groups = 'drop')
  }, error = function(e) {
    message("Error processing days of stay: ", e$message)
    return(NULL)
  })
})

totales_incidencia <- reactiveVal({
  raw_morbi_diabetes %>%
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
})

totales_mortalidad <- reactiveVal({
  raw_morta_diabetes %>% 
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
    summarise(count = sum(Dato), .groups = 'drop') %>%
    left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
    mutate(Dato = count/totales_poblacion*100000) %>%
    rename(Nombre_Unidad = nombre_unidad, Anio = anio)
})

data_censo_maestro <- reactiveVal({
  data <- load_data("SELECT * FROM CUUMS_MAESTRO")
  data
  
})

# These files need to be loaded from files as specified
citiesmx <- reactiveVal(read_csv("data/citiesmx.csv"))

# Define the file path and Google Drive link
shapefile_path <- "data/mun21gw/mun21gw.shp"

# Load municipalities shapefile with error handling
tryCatch({
  municipalities <- st_read(shapefile_path, quiet = TRUE)
  jalisco_shape <- municipalities %>%
    filter(NOM_ENT == "Jalisco")
}, error = function(e) {
  # Provide a helpful error message if the shapefile is missing
  message("Error loading shapefile: ", e$message)
  message("Please download the shapefile from: http://www.conabio.gob.mx/informacion/gis/maps/geo/mun21gw.zip")
  message("Extract the files and place them in the data/mun21gw/ folder")
  
  # Create empty sf objects as placeholders
  municipalities <- st_sf(data.frame(NOM_ENT = character(0), geometry = st_sfc()))
  jalisco_shape <- st_sf(data.frame(NOM_ENT = character(0), geometry = st_sfc()))
})

cat_ind <- read_csv("data/cat_indi_dm.csv", locale = locale(encoding = "utf-8"))

data_indicadores <- reactiveVal({
  # Load the catalog table with reference values
  cat_indicadores <- load_data("SELECT * FROM tb_cat_historico_indicadores")
  
  data <- load_data("SELECT * FROM tb_datos_historico_indicadores") %>%
  #data %>%
    select(-nombre_unidad)%>%
    filter(digito_equivalencia %in% c(558, 470, 5, 1, 8, 9, 7, 3, 6)) %>%
    left_join(select(cuums, ClavePresupuestal, DenominacionUnidad), by=c("clave" = "ClavePresupuestal"))%>% 
    left_join(cat_ind, by=join_by(nom_indicador==codigo)) %>%
    left_join(select(cat_indicadores, nom_indicador, anio, valor_ref, rango_esperado), by = c("nom_indicador" = "nom_indicador", "anio" = "anio")) %>%
    mutate(nombre_unidad = case_when(
      clave == "00" ~ "Nacional",
      clave == "14" ~ "Jalisco",
      TRUE ~ DenominacionUnidad
    )) %>% 
    mutate(desc_indicador = paste(nom_indicador, " - ", desc_indicador),
           valor_ref = as.numeric(valor_ref),
           lugar_esperado = ifelse(startsWith(rango_esperado, ">"), "mayor", "menor")) #%>% head()
  data
})

# Close connection when Shiny app stops
onStop(function() {
  if(exists("connection_details") && dbIsValid(connection_details)) {
    dbDisconnect(connection_details)
  }
})
