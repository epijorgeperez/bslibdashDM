# Proyecto CIIMSS Diabetes
Proyecto de análisis de datos para prevención, diagnóstico y control de la diabetes en el IMSS.

Revisa la documentación del proyecto aquí -> https://imssmx-my.sharepoint.com/:f:/g/personal/aide_gonzalezc_imss_gob_mx/Eq_okur0nZ1BqZg9csSlZDUBtK2kRXHW56WN9N0E9HqaJg?e=5%3aBdayXw&fromShare=true&at=9

y aquí -> https://github.com/epijorgeperez/bslibdashDM/blob/main/Documentos.md

![image](https://github.com/epijorgeperez/CIIMSS-Diabetes/assets/69016243/1e7423ab-381c-4459-9251-6277045d8701)

## Configuración de Base de Datos

Esta aplicación se conecta automáticamente a la base de datos del IMSS usando diferentes drivers según el entorno:

- **Windows**: Microsoft ODBC Driver for SQL Server
- **Linux/Docker**: FreeTDS driver
- **Auto-detección**: Detecta automáticamente el driver disponible

### Variables de Entorno Requeridas

Crea un archivo `.env` basado en `.env.example` con las siguientes variables:

```bash
# Database Configuration
DB_DRIVER_TYPE=auto          # auto, mssql, freetds
DB_USER=tu_usuario
DB_PASSWORD=tu_contraseña
DB_NAME=DAS_DM              # Base de datos (opcional, por defecto DAS_DM)
```

**Nota**: El servidor de base de datos (11.33.41.96) está configurado automáticamente.

## Ejecución Local

### Tablero Shiny (Desarrollo Local)

Para correr el dashboard localmente: 

1) Abre tu terminal (powershell, bash) y clona el repositorio: 
   ```bash
   git clone https://github.com/epijorgeperez/bslibdashDM.git
   ```

2) **Importante:** Descarga el archivo de shapefile de municipios de México desde: http://www.conabio.gob.mx/informacion/gis/maps/geo/mun21gw.zip 
   - Descomprime el archivo ZIP
   - Coloca el archivo mun21gw.shp en la carpeta `data/mun21gw/` de tu proyecto

3) Configura las variables de entorno:
   ```bash
   # Copia el archivo de ejemplo
   cp .env.example .env
   
   # Edita .env con tus credenciales
   nano .env  # o tu editor preferido
   ```

4) En tu terminal de R instala las librerías y paquetes necesarios con:
   ```r
   renv::restore()
   ```

5) En tu terminal de R cambia tu directorio de trabajo:
   ```r
   setwd("./bslibdashDM")
   ```

6) Llama a la librería de shiny:
   ```r
   library(shiny)
   ```

7) Corre la app:
   ```r
   runApp()
   ```

## Ejecución con Docker

### Requisitos Previos

- Docker y Docker Compose instalados
- Archivo `.env` configurado con credenciales

### Despliegue

1) Clona el repositorio:
   ```bash
   git clone https://github.com/epijorgeperez/bslibdashDM.git
   cd bslibdashDM
   ```

2) Configura variables de entorno:
   ```bash
   cp .env.example .env
   # Edita .env con tus credenciales
   ```

3) Construye y ejecuta el contenedor:
   ```bash
   cd deploy
   docker-compose up -d --build
   ```

4) Accede a la aplicación en: http://localhost:3838

### Comandos Útiles Docker

```bash
# Ver logs
docker-compose logs -f bslibdashdm

# Reiniciar aplicación (cambios en código R)
docker-compose restart bslibdashdm

# Reconstruir imagen (cambios en Dockerfile)
docker-compose down
docker-compose up -d --build

# Parar aplicación
docker-compose down
```

## Configuración Multi-Entorno

La aplicación detecta automáticamente el entorno y usa el driver de base de datos apropiado:

### Windows (Desarrollo Local)
- Usa Microsoft ODBC Driver 18/17 for SQL Server
- Configuración automática de SSL/TLS

### Linux/Docker (Producción)
- Usa FreeTDS driver
- Configuración optimizada para contenedores

### Variables de Entorno Disponibles

| Variable | Descripción | Valores | Por Defecto |
|----------|-------------|---------|-------------|
| `DB_DRIVER_TYPE` | Tipo de driver | `auto`, `mssql`, `freetds` | `auto` |
| `DB_USER` | Usuario de base de datos | string | **requerido** |
| `DB_PASSWORD` | Contraseña de base de datos | string | **requerido** |
| `DB_NAME` | Nombre de base de datos | string | `DAS_DM` |

## Solución de Problemas

### Error de Conexión a Base de Datos

1. **Verificar credenciales**: Asegúrate de que `DB_USER` y `DB_PASSWORD` sean correctos
2. **Verificar conectividad**: Desde el servidor, verifica conectividad a 11.33.41.96:1433
3. **Verificar drivers**: Ejecuta `odbc::odbcListDrivers()` en R para ver drivers disponibles

### Error en Docker

1. **Reconstruir imagen**: `docker-compose up -d --build`
2. **Ver logs detallados**: `docker-compose logs -f bslibdashdm`
3. **Verificar variables**: `docker-compose config` para ver configuración

### Cambios en Código

- **Archivos R** (`source/`): Solo reiniciar contenedor
- **Configuración** (`Dockerfile`, etc.): Reconstruir imagen

## Arquitectura

```
bslibdashDM/
├── config/
│   └── database.R          # Lógica de conexión multi-driver
├── deploy/
│   ├── docker-compose.yml  # Configuración Docker
│   └── Dockerfile          # Imagen de contenedor
├── source/                 # Código fuente Shiny
│   ├── app.R
│   ├── global.R
│   └── ...
├── .env.example           # Template de variables
└── README.md
```
