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


mortalidad <- load_data("SELECT * FROM dbo.MORTA_DIABETES")
write_csv(mortalidad, "data/tb_morta_dm.csv")

incid <- load_data("SELECT * FROM MORBI_DIABETES")
write_csv(incid, "data/tb_incidencia_dm.csv")

ind_historico <- load_data("SELECT * FROM tb_datos_historico_indicadores")
write_csv(ind_historico, "data/tb_datos_historico_indicadores.csv")

cuums <- load_data("SELECT * FROM CUUMS_MAESTRO")
write_csv(cuums, "data/cuums_maestro.csv")

pob <- load_data("SELECT * FROM tb_poblacion")
write_csv(pob, "data/tb_poblacion.csv")

consultas <- load_data("SELECT * FROM tb_consulta_dm")
write_csv(consultas, "data/tb_consulta_dm.csv")

egresos <- load_data("SELECT * FROM dbo.tb_egreso_dm")
write_csv(egresos, "data/tb_egreso_dm.csv")

censo_prev <- load_data("SELECT * FROM dbo.tb_censo_DM")
write_csv(censo_prev, "data/tb_censo_DM.csv")

incapacidades <- load_data("SELECT * FROM dbo.tb_dm_incap")
write_csv(incapacidades, "data/tb_dm_incap.csv")
