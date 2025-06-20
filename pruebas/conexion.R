library(readr)
library(dplyr)
library(DBI)
library(odbc)

# Connection string to establish a connection to the SQL Server database
#connection_details <- dbConnect(odbc::odbc(), 
#                                Driver = "SQL Server", 
#                                Server = "11.33.41.96", 
#                                Database = "DAS_DM", 
#                                Trusted_Connection = "Yes")

# Function to load data from the database
#load_data <- function(query) {
#  data <- dbGetQuery(connection_details, query)
 # return(data)
#}

#
# Initialize connection_details to NULL before attempting connection
connection_details <- NULL

# Configuración fija
DB_SERVER <- "11.33.41.96"
DB_NAME <- "DAS_DM"

# Credenciales desde variables de entorno
DB_USER <- Sys.getenv("DB_USER", "")
DB_PASSWORD <- Sys.getenv("DB_PASSWORD", "")

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

# Descargar datos

mortalidad <- load_data("SELECT * FROM dbo.MORTA_DIABETES")
#write_csv(mortalidad, "data/tb_morta_dm.csv")

incid <- load_data("SELECT * FROM MORBI_DIABETES")
#write_csv(incidencia, "data/tb_incidencia_dm.csv")

ind_historico <- load_data("SELECT * FROM tb_datos_historico_indicadores")
#write_csv(ind_historico, "data/tb_datos_historico_indicadores.csv")

cuums <- load_data("SELECT * FROM CUUMS_MAESTRO")
#write_csv(cuums, "data/cuums_maestro.csv")

pob <- load_data("SELECT * FROM tb_poblacion")
#write_csv(pob, "data/tb_poblacion.csv")

consultas <- load_data("SELECT * FROM tb_consulta_dm")
#write_csv(consultas, "data/tb_consulta_dm.csv")

egresos <- load_data("SELECT * FROM dbo.tb_egreso_dm")
#write_csv(egresos, "data/tb_egreso_dm.csv")

censo_prev <- load_data("SELECT * FROM dbo.tb_censo_DM")
#write_csv(censo_prev, "data/tb_censo_DM.csv")

incapacidades <- load_data("SELECT * FROM dbo.tb_dm_incap")
#write_csv(incapacidades, "data/tb_dm_incap.csv")