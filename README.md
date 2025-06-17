# Proyecto CIIMSS Diabetes
Proyecto de análisis de datos para prevención, diagnóstico y control de la diabetes en el IMSS.

Revisa la documentación del proyecto aquí -> https://imssmx-my.sharepoint.com/:f:/g/personal/aide_gonzalezc_imss_gob_mx/Eq_okur0nZ1BqZg9csSlZDUBtK2kRXHW56WN9N0E9HqaJg?e=5%3aBdayXw&fromShare=true&at=9

y aquí -> https://github.com/epijorgeperez/bslibdashDM/blob/main/Documentos.md

![Dashboard Preview](https://cdn.abacus.ai/images/25824478-1869-4969-ba5d-3039684eba74.png)

## 🔧 Configuración de Base de Datos

Esta aplicación se conecta automáticamente a la base de datos del IMSS detectando el driver ODBC disponible en tu sistema.

### Variables de Entorno Requeridas

Crea un archivo `.env` basado en `.env.example` con las siguientes variables:

```bash
# Database Configuration
DB_USER=tu_usuario_imss
DB_PASSWORD=tu_contraseña
DB_NAME=DAS_DM


Nota: El servidor de base de datos (11.33.41.96) está configurado automáticamente.

💻 Desarrollo Local
Requisitos Previos
R 4.5.0+
RStudio (recomendado)
Microsoft ODBC Driver 17 o 18 for SQL Server
Acceso a red IMSS para conexión a base de datos
Configuración e Instalación

Clona el repositorio:

git clone https://github.com/epijorgeperez/bslibdashDM.git
cd bslibdashDM


Descarga el shapefile de municipios:

Descarga: http://www.conabio.gob.mx/informacion/gis/maps/geo/mun21gw.zip
Descomprime el archivo ZIP
Coloca el archivo mun21gw.shp en la carpeta data/mun21gw/ de tu proyecto

Configura las variables de entorno:

# Copia el archivo de ejemplo
cp .env.example .env

# Edita .env con tus credenciales IMSS
# Puedes usar cualquier editor de texto


Instala las dependencias de R:

# En RStudio o consola de R
renv::restore()


Ejecuta la aplicación:

# Cargar librería
library(shiny)

# Ejecutar aplicación
runApp()

Instalación de ODBC Driver (Windows)

Si no tienes el driver ODBC instalado:

Descarga Microsoft ODBC Driver 17 for SQL Server desde: https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server

Instala el driver siguiendo las instrucciones del instalador

Verifica la instalación en R:

odbc::odbcListDrivers()

🛠️ Solución de Problemas
Error de Conexión a Base de Datos

Verificar credenciales:

Asegúrate de que DB_USER y DB_PASSWORD en .env sean correctos
Las credenciales deben ser las mismas que usas para acceder a sistemas IMSS

Verificar conectividad de red:

Debes estar conectado a la red IMSS
Verifica que puedas acceder a otros sistemas internos del IMSS

Verificar driver ODBC:

# En R, ejecuta:
odbc::odbcListDrivers()
# Debe mostrar "ODBC Driver 17 for SQL Server" o similar

Problemas de Carga
Tiempo de carga: La aplicación tarda 5-8 minutos en cargar inicialmente debido al volumen de datos (1M+ registros)
Memoria: Asegúrate de tener al menos 4GB de RAM disponible
Paciencia: No cierres la aplicación durante la carga inicial
Errores de Paquetes

Si hay errores relacionados con paquetes faltantes:

# Reinstalar dependencias
renv::restore()

# Si persiste el problema, limpiar y reinstalar
renv::clean()
renv::restore()

Variables de Entorno

Si la aplicación no encuentra las variables de entorno:

Verifica que el archivo .env esté en la raíz del proyecto
Verifica que no tenga espacios extra o caracteres especiales
Reinicia RStudio después de crear/modificar .env
📊 Especificaciones Técnicas
Rendimiento
Tiempo de carga inicial: 5-8 minutos
Registros procesados: 1M+ filas de datos IMSS
Memoria recomendada: 4GB+ RAM
Requisitos de red: Conexión a red IMSS
Dependencias Principales
R 4.5.0+
Shiny & bslib (interfaz)
DBI & odbc (base de datos)
dplyr (manipulación de datos)
plotly & leaflet (visualizaciones)
🏗️ Estructura del Proyecto
bslibdashDM/
├── global.R               # Configuración global y conexión DB
├── app.R                  # Aplicación principal Shiny
├── data/                  # Datos estáticos
│   └── mun21gw/          # Shapefiles de municipios (descargar)
├── .env.example          # Template de variables de entorno
├── .env                  # Variables de entorno (crear)
├── renv.lock             # Dependencias R
└── README.md

📞 Soporte

Para problemas técnicos:

Revisa la sección de Solución de Problemas
Consulta la documentación en SharePoint
Verifica que tengas acceso a la red IMSS
Contacta al equipo de desarrollo

Nota: Esta aplicación requiere acceso a la red interna del IMSS para funcionar correctamente.