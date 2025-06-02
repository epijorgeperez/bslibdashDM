FROM rocker/shiny:latest

# Configurar repositorio CRAN HTTPS
RUN echo "options(repos = c(CRAN='https://cloud.r-project.org'))" \
    >> /usr/local/lib/R/etc/Rprofile.site

# Actualizar repositorios y instalar dependencias básicas
RUN apt-get update && apt-get install -y \
    software-properties-common \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Instalar dependencias del sistema esenciales
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    curl \
    gnupg \
    unixodbc \
    unixodbc-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Instalar dependencias geoespaciales (para sf, leaflet)
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Dependencias para texto y gráficos (gt, ggplot2)
RUN apt-get update && apt-get install -y \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libfontconfig1-dev \
    cmake \
    build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Instalar Microsoft ODBC Driver para SQL Server (Ubuntu 22.04)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
    && curl https://packages.microsoft.com/config/ubuntu/22.04/prod.list > /etc/apt/sources.list.d/mssql-release.list \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y msodbcsql18 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Copiar el código de la aplicación
COPY ./source/ /srv/shiny-server/bslibdashdm/

# Cambiar permisos para usuario shiny
RUN chown -R shiny:shiny /srv/shiny-server/bslibdashdm

# Instalar renv y restaurar paquetes
RUN R -e "install.packages('renv')"
WORKDIR /srv/shiny-server/bslibdashdm
RUN R -e "renv::restore(confirm=FALSE)"

# Configurar Shiny Server
RUN echo "run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    app_dir /srv/shiny-server/bslibdashdm;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}" > /etc/shiny-server/shiny-server.conf

# Exponer puerto
EXPOSE 3838

# Comando para iniciar Shiny Server
CMD ["/usr/bin/shiny-server"]