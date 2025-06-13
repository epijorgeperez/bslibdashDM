# config/database.R
# Configuración de conexión a base de datos multi-driver
# Servidor fijo: 11.33.41.96, Base de datos: DAS_DM

get_database_connection <- function() {
  # Configuración fija del servidor
  db_server <- "11.33.41.96"
  db_name <- "DAS_DM"
  
  # Credenciales desde variables de entorno
  db_user <- Sys.getenv("DB_USER", "")
  db_password <- Sys.getenv("DB_PASSWORD", "")
  db_driver_type <- Sys.getenv("DB_DRIVER_TYPE", "auto")
  
  message("=== Database Connection Setup ===")
  message("Server: ", db_server)
  message("Database: ", db_name)
  message("Driver type: ", db_driver_type)
  message("SQL Auth user provided: ", ifelse(db_user != "", "YES", "NO"))
  message("SQL Auth password provided: ", ifelse(db_password != "", "YES", "NO"))
  
  # Validar credenciales
  if (db_user == "" || db_password == "") {
    stop("DB_USER and DB_PASSWORD environment variables are required")
  }
  
  # Auto-detectar driver disponible si no se especifica
  if (db_driver_type == "auto") {
    db_driver_type <- detect_available_driver()
  }
  
  # Intentar conexión según el tipo de driver
  connection <- switch(db_driver_type,
    "mssql" = connect_with_mssql_driver(db_server, db_name, db_user, db_password),
    "freetds" = connect_with_freetds(db_server, db_name, db_user, db_password),
    stop("Unsupported database driver type: ", db_driver_type)
  )
  
  # Verificar conexión
  if (dbIsValid(connection)) {
    message("✅ Database connection established successfully with ", db_driver_type)
    return(connection)
  } else {
    stop("❌ Database connection failed")
  }
}

# Detectar qué drivers están disponibles
detect_available_driver <- function() {
  available_drivers <- odbc::odbcListDrivers()$name
  
  message("Available ODBC drivers:")
  for(driver in unique(available_drivers)) {
    message("  - ", driver)
  }
  
  # Prioridad: MSSQL > FreeTDS
  if (any(grepl("ODBC Driver.*SQL Server", available_drivers, ignore.case = TRUE))) {
    message("🔍 Auto-detected: Microsoft SQL Server driver")
    return("mssql")
  } else if (any(grepl("FreeTDS", available_drivers, ignore.case = TRUE))) {
    message("🔍 Auto-detected: FreeTDS driver")
    return("freetds")
  } else {
    stop("❌ No compatible SQL Server drivers found. Available: ", 
         paste(unique(available_drivers), collapse = ", "))
  }
}

# Conexión con Microsoft SQL Server Driver
connect_with_mssql_driver <- function(server, database, user, password) {
  available_drivers <- odbc::odbcListDrivers()$name
  
  # Detectar versión del driver disponible
  if (any(grepl("ODBC Driver 18", available_drivers))) {
    driver_name <- "ODBC Driver 18 for SQL Server"
    message("🔧 Using: ", driver_name, " with SSL workarounds")
    
    # Intentar diferentes configuraciones SSL para Driver 18
    connection_attempts <- list(
      # Intento 1: Deshabilitar SSL completamente
      list(
        Driver = driver_name,
        Server = server,
        Database = database,
        UID = user,
        PWD = password,
        TrustServerCertificate = "yes",
        Encrypt = "no",
        encoding = "UTF-8"
      ),
      # Intento 2: SSL opcional
      list(
        Driver = driver_name,
        Server = server,
        Database = database,
        UID = user,
        PWD = password,
        TrustServerCertificate = "yes",
        Encrypt = "optional",
        encoding = "UTF-8"
      ),
      # Intento 3: Configuración básica
      list(
        Driver = driver_name,
        Server = server,
        Database = database,
        UID = user,
        PWD = password,
        encoding = "UTF-8"
      )
    )
    
  } else if (any(grepl("ODBC Driver 17", available_drivers))) {
    driver_name <- "ODBC Driver 17 for SQL Server"
    message("🔧 Using: ", driver_name)
    
    connection_attempts <- list(
      list(
        Driver = driver_name,
        Server = server,
        Database = database,
        UID = user,
        PWD = password,
        encoding = "UTF-8"
      )
    )
    
  } else {
    # Fallback a driver genérico
    driver_name <- "SQL Server"
    message("🔧 Using: ", driver_name, " (generic)")
    
    connection_attempts <- list(
      list(
        Driver = driver_name,
        Server = server,
        Database = database,
        UID = user,
        PWD = password,
        encoding = "UTF-8"
      )
    )
  }
  
  # Intentar conexiones en orden
  for (i in seq_along(connection_attempts)) {
    tryCatch({
      message("🔄 MSSQL connection attempt ", i, "/", length(connection_attempts))
      connection <- do.call(dbConnect, c(list(odbc::odbc()), connection_attempts[[i]]))
      return(connection)
    }, error = function(e) {
      message("❌ Attempt ", i, " failed: ", e$message)
      if (i == length(connection_attempts)) {
        stop("All MSSQL connection attempts failed. Last error: ", e$message)
      }
    })
  }
}

# Conexión con FreeTDS
connect_with_freetds <- function(server, database, user, password) {
  message("🔧 Using: FreeTDS driver")
  
  # Intentar con servername configurado primero (tu configuración actual)
  tryCatch({
    message("🔄 FreeTDS attempt 1: Using configured servername 'testserver'")
    connection <- dbConnect(odbc::odbc(),
      driver = "FreeTDS",
      servername = "testserver",  # Servidor configurado en freetds.conf
      uid = user,
      pwd = password,
      encoding = "UTF-8"
    )
    return(connection)
  }, error = function(e1) {
    message("❌ Servername method failed: ", e1$message)
    
    # Fallback a conexión directa
    tryCatch({
      message("🔄 FreeTDS attempt 2: Direct connection to IP")
      connection <- dbConnect(odbc::odbc(),
        driver = "FreeTDS",
        server = server,
        database = database,
        uid = user,
        pwd = password,
        port = 1433,
        encoding = "UTF-8"
      )
      return(connection)
    }, error = function(e2) {
      stop("All FreeTDS connection attempts failed. Last error: ", e2$message)
    })
  })
}

# Función de utilidad para probar la conexión
test_connection <- function(connection) {
  if (is.null(connection) || !dbIsValid(connection)) {
    return(FALSE)
  }
  
  tryCatch({
    result <- dbGetQuery(connection, "SELECT 1 AS test")
    return(nrow(result) == 1 && result$test == 1)
  }, error = function(e) {
    message("Connection test failed: ", e$message)
    return(FALSE)
  })
}