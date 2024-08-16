library(readr)
library(dplyr)
library(DBI)
library(odbc)

# Connection string to establish a connection to the SQL Server database
connection_details <- dbConnect(odbc::odbc(), 
                                Driver = "SQL Server", 
                                Server = "11.33.41.96", 
                                Database = "DAS_DM", 
                                Trusted_Connection = "Yes")

# Function to load data from the database
load_data <- function(query) {
  data <- dbGetQuery(connection_details, query)
  return(data)
}


# Example of loading specific tables
 pob <- load_data("SELECT * FROM tb_poblacion")
 #consultas <- load_data("SELECT * FROM tb_consulta_dm")
 #indicadores <- load_data("SELECT * FROM dbo.tb_datos_historico_indicadores")
 incapacidades <- load_data("SELECT * FROM dbo.tb_dm_incap")
 #poblacion <- load_data("SELECT * FROM dbo.tb_poblacion")
mortalidad <- load_data("SELECT * FROM dbo.MORTA_OOAD")
incid <- load_data("SELECT * FROM MORBI_DM WHERE cve_delega = 14")
cuums <- load_data("SELECT * FROM CUUMS_MAESTRO")

#censo <- load_data("SELECT * FROM tb_poblacion")
write_csv(cuums, "data/cuums_maestro.csv")
#write_csv(incidencia, "data/tb_incidencia_dm.csv")