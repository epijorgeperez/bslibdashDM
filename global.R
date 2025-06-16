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


#Datos para gráficos
message("Loading data_censo...")
data_censo <- reactiveVal({
  message("Executing data_censo query...")
  data <- load_data("SELECT * FROM dbo.tb_censo_DM") %>%
            rename(Dato= Pacientes_DM) #Pacientes_DM -> Numero de pacientes Prevalencia_DM -> prevalencia
  message("data_censo loaded successfully")
  data

})

message("Loading data_consulta...")
data_consulta <- reactiveVal({
  message("Executing data_consulta query...")
  data <- load_data("SELECT * FROM tb_consulta_dm")
  message("data_consulta loaded successfully")
  data

})

message("Loading data_incapacidad...")
data_incapacidad <- reactiveVal({
  tryCatch({
    message("Executing data_incapacidad query...")
    data <- load_data("SELECT * FROM dbo.tb_dm_incap") %>%
      # Convert character columns to numeric right after loading
      # Replace NAs from conversion errors with 0
      mutate(NDIAS = as.numeric(NDIAS),
             NDIAS = ifelse(is.na(NDIAS), 0, NDIAS),
             FREC = as.numeric(FREC),
             FREC = ifelse(is.na(FREC), 0, FREC)) %>%
      rename(Anio = PERIODO, Nombre_Unidad = descnivel, Grupo_edad = descgedad, Sexo = TIP_SEXO)%>%
      group_by(Anio, Nombre_Unidad, NIVEL, Grupo_edad, Sexo) %>%
        summarise(Dato = sum(NDIAS, na.rm = TRUE), 
                  Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                  .groups = 'drop') %>%
        mutate(Nombre_Unidad = case_when(is.na(Nombre_Unidad) ~ NA_character_,
              Nombre_Unidad == "14 Jalisco" ~ "Jalisco",
              Nombre_Unidad == "99 Nacional" ~ "Nacional",
              TRUE ~ str_trim(str_extract(Nombre_Unidad, "(?<=\\s).*"))))
    message("data_incapacidad loaded successfully")
    data
  }, error = function(e) {
    # Log error to console instead of using showNotification
    message("Error loading incapacidad data: ", e$message)
    return(NULL)
  })
})

message("Loading data_hosp...")
data_hosp <- reactiveVal({
  message("Executing data_hosp query...")
  data <- load_data("SELECT * FROM dbo.tb_egreso_dm")
  message("data_hosp loaded successfully")
  data
})


message("Loading data_incidencia...")
data_incidencia <- reactiveVal({
  message("Executing data_incidencia query...")
  data <- load_data("SELECT * FROM MORBI_DIABETES") %>%
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
  message("data_incidencia loaded successfully")
  data
})


data_mortalidad <- reactiveVal({
  data <- load_data("SELECT * FROM dbo.MORTA_DIABETES") %>%
    filter(Sexo != 0, Grupo_edad != "Total") %>%
    mutate(Cve_Presupuestal = case_when(
      Nombre_OOAD == "Nacional" ~ "00",
      Nombre_OOAD == "Jalisco" & is.na(Cve_Presupuestal) ~ "14",
      TRUE ~ Cve_Presupuestal
    )) %>%
    group_by(Cve_Presupuestal, Sexo, Anio, Grupo_edad) %>%
    summarise(Casos = sum(Dato), .groups = 'drop')%>%
    full_join(poblacion_gpoedad, by = c("Cve_Presupuestal", "Sexo", "Anio", "Grupo_edad")) %>%
    mutate(Casos = ifelse(is.na(Casos), 0, Casos)) %>%
    mutate(Dato = Casos/Totales_Poblacion * 100000) 
  data
})



## Datos para métricas 
#Prevalencia
totales_anuales <- reactiveVal({
    data <- load_data("SELECT * FROM dbo.tb_censo_DM")
    
    # Original code (commented out)
    #data <- read_csv("data/tb_censo_DM.csv")
    
    data %>% 
      filter (Sexo == 0) %>%
      mutate(Nombre_Unidad = ifelse(nchar(Cve_Presupuestal) < 3 & Cve_Presupuestal != "00", "OOAD", Nombre_Unidad)) %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
      summarise(Pacientes_DM = sum(Pacientes_DM, na.rm = TRUE), 
                PAMF = sum(PAMF, na.rm = TRUE),
                Dato = Pacientes_DM / PAMF * 100,
                .groups = 'drop')%>%
      filter(PAMF != 0)
})

#Consultas
totales_consultas <- reactiveVal({
    data <- load_data("SELECT * FROM tb_consulta_dm")
    
    data <- data %>% 
      filter(Parametro == 'Consulta_MF', Sexo == 0, Grupo_edad=="Total") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
      summarise(Dato = sum(Dato, na.rm = TRUE), .groups = 'drop') %>%
      rename(anio = Anio, cve_presupuestal = Cve_Presupuestal) %>%
      # Ensure anio is character type for consistent joining
      mutate(anio = as.character(anio),
             cve_presupuestal = as.character(cve_presupuestal)) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
      mutate(Dato2 = Dato/totales_poblacion*1000) %>%
      rename('Anio' = 'anio', 'Totales_Poblacion' = 'totales_poblacion')%>%
      select(Anio,Nombre_OOAD, Nombre_Unidad, Dato, Totales_Poblacion, Dato2)
})


totales_incap <- reactiveVal({
    tryCatch({
      data <- load_data("SELECT * FROM dbo.tb_dm_incap") %>%
        # Convert character columns to numeric right after loading
        # Replace NAs from conversion errors with 0
        mutate(NDIAS = as.numeric(NDIAS),
               NDIAS = ifelse(is.na(NDIAS), 0, NDIAS),
               FREC = as.numeric(FREC),
               FREC = ifelse(is.na(FREC), 0, FREC))
      
      # Original code (commented out)
      #data <- read_csv("data/tb_dm_incap.csv")
      
      data <- data %>% 
        rename(Anio = PERIODO, Nombre_Unidad = descnivel) %>%
        # Ensure Anio is character type for consistent joining
        mutate(Anio = as.character(Anio)) %>%
        group_by(Anio, Nombre_Unidad, NIVEL) %>%
        summarise(ndias = sum(NDIAS, na.rm = TRUE), 
                  Dato_prom = (sum(NDIAS, na.rm = TRUE)/sum(FREC, na.rm = TRUE)),
                  .groups = 'drop') %>%
        mutate(NIVEL = as.character(NIVEL)) %>%
        left_join(
          # Ensure anio in poblacion_totales_rt is also character type
          poblacion_totales_rt %>% 
            select(cve_prei, nombre_ooad, anio, totales_poblacion) %>%
            mutate(anio = as.character(anio)), 
          by = c('NIVEL' = 'cve_prei', "Anio" = "anio")
        ) %>%
        mutate(Dato = ndias/totales_poblacion*100) %>%
        ##cambiar este case_when para que funcione cuando se agreguen mas estados, se puede hacer
        ##un join con la tabla de estados que se puede extraer del cuums_maestro
        mutate(Nombre_Unidad = case_when(is.na(Nombre_Unidad) ~ NA_character_,
                                        str_detect(Nombre_Unidad, "^\\d{2} ") & !str_detect(Nombre_Unidad, "^99 ") ~ "OOAD",
                                        Nombre_Unidad == "99 Nacional" ~ "Nacional",
                                        TRUE ~ str_trim(str_extract(Nombre_Unidad, "(?<=\\s).*"))))
      data
    }, error = function(e) {
      # Log error to console instead of using showNotification
      message("Error loading incapacidad data: ", e$message)
      return(NULL)
    })
})

totales_hosp <- reactiveVal({
    tryCatch({
    data <- load_data("SELECT * FROM dbo.tb_egreso_dm")

    
    data %>%
      filter(Sexo == 0, Parametro == "Egresos_DM_Adsc", Especialidad == "Total", Grupo_edad=="Total") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad, Cve_Presupuestal) %>%
      summarise(Dato = sum(Dato, na.rm = TRUE),.groups = 'drop') %>%
      rename(anio = Anio, cve_presupuestal = Cve_Presupuestal) %>%
      # Ensure anio is character type for consistent joining
      mutate(anio = as.character(anio),
             cve_presupuestal = as.character(cve_presupuestal)) %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
      mutate(Dato2 = Dato/totales_poblacion*100000) %>%
      select(Anio = anio,Nombre_OOAD, Nombre_Unidad, Dato, totales_poblacion, Dato2)
  }, error = function(e) {
    # Log error to console instead of using showNotification
    message("Error loading hospitalization data: ", e$message)
    return(NULL)
  })
})

totales_dias_estancia<- reactiveVal({
    tryCatch({
    data <- load_data("SELECT * FROM dbo.tb_egreso_dm")
    

    data %>% 
      filter(Sexo == 0, Parametro == "DiasEstancia_DM_Adsc") %>%
      group_by(Anio, Nombre_OOAD, Nombre_Unidad) %>%
      summarise(Dato = mean(Dato, na.rm = TRUE),
                .groups = 'drop')
  }, error = function(e) {
    # Log error to console instead of using showNotification
    message("Error loading days of stay data: ", e$message)
    return(NULL)
  })
})

totales_incidencia <- reactiveVal({
  data <- load_data("SELECT * FROM MORBI_DIABETES")

  
  data <- data %>%
      filter(Grupo_edad == "TTotal", Sexo == 0) %>%
      mutate(Cve_Presupuestal = case_when(
      Nombre_OOAD == "Nacional" ~ "00",
      Nombre_OOAD == "Jalisco" & is.na(Cve_Presupuestal) ~ "14",
      TRUE ~ Cve_Presupuestal
    )) %>%
      rename(anio=Anio, cve_presupuestal= Cve_Presupuestal) %>%
      # Ensure anio is character type for consistent joining
      mutate(anio = as.character(anio),
             cve_presupuestal = as.character(cve_presupuestal)) %>%
      group_by(anio, cve_presupuestal) %>%
      summarise(Pacientes_DM = sum(Dato, na.rm = TRUE), .groups = "drop") %>%
      inner_join(poblacion_totales, by = c("anio", "cve_presupuestal")) %>%
      mutate(Dato = Pacientes_DM / totales_poblacion * 100000) %>%
      rename(Nombre_Unidad = nombre_unidad, Anio = anio)

  data
})

totales_mortalidad <- reactiveVal({
    mort <- load_data("SELECT * FROM dbo.MORTA_DIABETES")


    data <- mort %>% 
      filter(Grupo_edad == "Total", Sexo == 0) %>%
      mutate(Cve_Presupuestal = case_when(
      Nombre_OOAD == "Nacional" ~ "00",
      Nombre_OOAD == "Jalisco" & is.na(Cve_Presupuestal) ~ "14",
      TRUE ~ Cve_Presupuestal
    )) %>%
      rename(anio=Anio, cve_presupuestal= Cve_Presupuestal) %>%
      # Ensure anio is character type for consistent joining
      mutate(anio = as.character(anio),
             cve_presupuestal = as.character(cve_presupuestal)) %>%
      group_by(anio, cve_presupuestal) %>%
      summarise(count = sum(Dato), .groups = 'drop') %>%
      left_join(poblacion_totales, by = c("anio", "cve_presupuestal"))%>%
      mutate(Dato = count/totales_poblacion*100000) %>%
      rename(Nombre_Unidad = nombre_unidad, Anio = anio)
    data                
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
  data <- load_data("SELECT * FROM tb_datos_historico_indicadores") %>%
    select(-nombre_unidad)%>%
    filter(digito_equivalencia %in% c(558, 470, 5, 1, 8, 9, 7, 3, 6)) %>%
    left_join(select(cuums, ClavePresupuestal, DenominacionUnidad), by=c("clave" = "ClavePresupuestal"))%>% 
    left_join(cat_ind, by=join_by(nom_indicador==codigo)) %>%
    mutate(nombre_unidad = case_when(
      clave == "00" ~ "Nacional",
      clave == "14" ~ "Jalisco",
      TRUE ~ DenominacionUnidad
    )) %>%
    mutate(desc_indicador = paste(nom_indicador, " - ", desc_indicador))
  data
})

# Close connection when Shiny app stops
onStop(function() {
  if(exists("connection_details") && dbIsValid(connection_details)) {
    dbDisconnect(connection_details)
  }
})
