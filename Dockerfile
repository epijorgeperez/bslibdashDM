# Se utiliza la imagen base de rocker para Shiny
FROM rocker/shiny:latest

# =============================================================================
# --- 1. INSTALACIÓN DE DEPENDENCIAS DEL SISTEMA (BLOQUE CONSOLIDADO) ---
# Se instalan todas las dependencias en una sola capa para optimizar el tamaño
# y la velocidad de construcción de la imagen.
# =============================================================================
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Dependencias básicas y de red
    software-properties-common \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    curl \
    gnupg \
    ca-certificates \
    # Dependencias para ODBC
    unixodbc \
    unixodbc-dev \
    # Dependencias geoespaciales (para sf, leaflet, etc.)
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    # Dependencias para texto y gráficos (gt, ggplot2, etc.)
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libfontconfig1-dev \
    cmake \
    build-essential \
    # Dependencias para FreeTDS
    freetds-bin \
    freetds-dev \
    tdsodbc \
    # Limpieza final para reducir el tamaño de la capa
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# --- 2. INSTALACIÓN DEL DRIVER ODBC DE MICROSOFT ---
# Se sigue el método moderno y seguro para agregar repositorios y llaves GPG.
# =============================================================================
RUN curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor -o /usr/share/keyrings/microsoft-prod.gpg \
    && curl -fsSL https://packages.microsoft.com/config/ubuntu/22.04/prod.list > /etc/apt/sources.list.d/mssql-release.list \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y --no-install-recommends msodbcsql18 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# --- 3. CONFIGURACIÓN DE ODBC Y FREETDS ---
# =============================================================================
# Configurar los drivers para que R y otras aplicaciones los encuentren.
RUN echo '[ODBC Driver 18 for SQL Server]\n\
Description=Microsoft ODBC Driver 18 for SQL Server\n\
Driver=/opt/microsoft/msodbcsql18/lib64/libmsodbcsql-18.5.so.1.1\n\
UsageCount=1\n\
\n\
[FreeTDS]\n\
Description=FreeTDS Driver\n\
Driver=/usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so\n\
Setup=/usr/lib/x86_64-linux-gnu/odbc/libtdsS.so\n\
UsageCount=1' > /etc/odbcinst.ini


# Configurar FreeTDS.
# IMPORTANTE: No se deben "hardcodear" IPs en un Dockerfile.
# Esta configuración debe ser manejada con variables de entorno o archivos de
# configuración montados en el contenedor en tiempo de ejecución.
RUN echo '[testserver]\n\
    host = 11.33.41.96\n\
    port = 1433\n\
    tds version = 8.0' > /etc/freetds/freetds.conf

# =============================================================================
# --- 4. CONFIGURACIÓN DE RENV Y RESTAURACIÓN DE PAQUETES DE R ---
# Esta es la sección clave para resolver el problema de renv.
# =============================================================================
# Definir el usuario que ejecutará la app de Shiny
ARG APP_USER=shiny

# Forzar a renv a usar un caché de paquetes global y compartido
ENV RENV_PATHS_CACHE /opt/renv/cache

# Crear el directorio del caché y darle permisos al usuario de la app
RUN mkdir -p ${RENV_PATHS_CACHE} && chown -R ${APP_USER}:${APP_USER} /opt/renv

# Establecer el directorio de trabajo para la aplicación
WORKDIR /srv/shiny-server/bslibdashdm

# Copiar SOLO los archivos de renv para optimizar la caché de capas de Docker.
# NO se copia el directorio renv/ de la máquina local para evitar problemas de symlinks.
COPY ./source/renv.lock ./
COPY ./source/.Rprofile ./

# Instalar renv y restaurar los paquetes. Esta capa se reutilizará si renv.lock no cambia.
# Se usa el RENV_PATHS_CACHE que se definió globalmente.
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" && \
    R -e "renv::restore()"

# =============================================================================
# --- 5. CONFIGURACIÓN FINAL DE LA APLICACIÓN SHINY ---
# =============================================================================
# Ahora se copia el resto del código fuente de la aplicación
COPY ./source/ ./

# Asegurarse de que toda la aplicación pertenezca al usuario 'shiny'
RUN chown -R ${APP_USER}:${APP_USER} /srv/shiny-server

# Configurar el servidor Shiny para que ejecute la app desde el directorio correcto
RUN echo "run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    app_dir /srv/shiny-server/bslibdashdm;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}" > /etc/shiny-server/shiny-server.conf

# Exponer el puerto de la aplicación
EXPOSE 3838

# Cambiar al usuario final que ejecutará el proceso del servidor
USER ${APP_USER}

# Comando para iniciar el servidor Shiny
CMD ["/usr/bin/shiny-server"]