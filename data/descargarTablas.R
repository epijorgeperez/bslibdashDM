library(DBI)
library(odbc)
library(readr)

# Connection string to establish a connection to the SQL Server database
connection_details <- dbConnect(odbc::odbc(), 
                                Driver = "SQL Server", 
                                Server = "MTO02438WSCHG7", 
                                Database = "DAS_DM", 
                                Trusted_Connection = "Yes")

# Function to load data from the database
load_data <- function(query) {
  data <- dbGetQuery(connection_details, query)
  return(data)
}

# Example of loading specific tables
 censo <- load_data("SELECT * FROM dbo.tb_censo_DM")
 consultas <- load_data("SELECT * FROM tb_consulta_dm")
 indicadores <- load_data("SELECT * FROM dbo.tb_datos_historico_indicadores")
 incapacidades <- load_data("SELECT * FROM dbo.tb_dm_incap")
 poblacion <- load_data("SELECT * FROM dbo.tb_poblacion")
 egresos <- load_data("SELECT * FROM dbo.tb_egreso_dm")


write_csv(egresos, "data/tb_egreso_dm.csv")
