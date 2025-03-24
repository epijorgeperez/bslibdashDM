## Proyecto CIIMSS Diabetes
Proyecto de analisis de datos para prevención, diagnóstico y control de la diabetes en el IMSS.



Revisa la documentación del proyecto aquí -> https://imssmx-my.sharepoint.com/:f:/g/personal/aide_gonzalezc_imss_gob_mx/Eq_okur0nZ1BqZg9csSlZDUBtK2kRXHW56WN9N0E9HqaJg?e=5%3aBdayXw&fromShare=true&at=9

y aquí -> https://github.com/epijorgeperez/bslibdashDM/blob/main/Documentos.md

![image](https://github.com/epijorgeperez/CIIMSS-Diabetes/assets/69016243/1e7423ab-381c-4459-9251-6277045d8701)


### Tablero Shiny

![image](https://github.com/user-attachments/assets/fe8156da-2ddc-4f28-83f6-89c543f5394e)


Para correr el dashboard localmente: 

1) Abre tu terminal (powershell, bash) y clona el repositorio: ``` git clone https://github.com/epijorgeperez/bslibdashDM.git ```

2) **Importante:** Descarga el archivo de shapefile de municipios de México desde: http://www.conabio.gob.mx/informacion/gis/maps/geo/mun21gw.zip 
   - Descomprime el archivo ZIP
   - Coloca el archivo mun21gw.shp en la carpeta `data/mun21gw/` de tu proyecto

3) En tu terminal de R Instala las librerías y paquetes necesarios con ``` renv::restore ```

4) En tu terminal de R cambia tu directorio de trabajo ``` setwd("./bslibdashDM") ```

5) Llama a la librería de shiny:  ``` library(shiny) ```.

6) Corre la app: ``` runApp() ```

