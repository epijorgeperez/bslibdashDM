---
title: "Dashboard de Diabetes IMSS"
subtitle: "Análisis Epidemiológico Integral del Comportamiento de la Diabetes en el IMSS"
author: "CIIMSS - Centro de Inteligencia en Salud del IMSS"
date: "`r Sys.Date()`"
output:
  revealjs::revealjs_presentation:
    theme: white
    highlight: github
    center: true
    transition: slide
    background_transition: fade
    reveal_options:
      slideNumber: true
      previewLinks: true
    css: custom.css
    self_contained: true
---

```{css, echo=FALSE}
/* IMSS Color Palette */
:root {
  --imss-green: #006456;
  --imss-gold: #AE851E;
  --imss-burgundy: #691A32;
  --imss-red: #A02042;
}

.reveal h1, .reveal h2, .reveal h3 {
  color: var(--imss-green);
  text-transform: none;
}

.reveal h2 {
  color: var(--imss-gold);
  border-bottom: 3px solid var(--imss-gold);
  padding-bottom: 10px;
}

.hero-slide {
  background: linear-gradient(135deg, var(--imss-green) 0%, #004c44 100%);
  color: white !important;
}

.hero-slide h1, .hero-slide h2 {
  color: white !important;
}

.dimension-container {
  display: flex;
  justify-content: space-around;
  gap: 1rem;
  margin: 2rem 0;
}

.dimension-box {
  text-align: center;
  padding: 1rem;
  background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
  border-radius: 10px;
  border: 2px solid var(--imss-green);
  flex: 1;
}

.metric-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 1rem;
  font-size: 0.8em;
}

.metric-card {
  background: white;
  border-radius: 8px;
  padding: 1rem;
  border-left: 4px solid var(--imss-burgundy);
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
}

.two-columns {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 2rem;
  text-align: left;
}

.status-yes { color: #28a745; font-weight: bold; }
.status-no { color: #dc3545; font-weight: bold; }
```

# {.hero-slide}

## Dashboard de Diabetes IMSS

### Análisis Epidemiológico Integral del Comportamiento de la Diabetes en el IMSS

<br>

**CIIMSS - Centro de Inteligencia en Salud del IMSS**

## 🎯 Objetivo del Dashboard

Este tablero tiene como objetivo principal **conocer la situación actual de la Diabetes en el IMSS** desde el punto de vista de su comportamiento epidemiológico.

<div class="dimension-container">
<div class="dimension-box">
<div style="font-size: 3rem;">⏰</div>
<strong>TIEMPO</strong>
</div>
<div class="dimension-box">
<div style="font-size: 3rem;">📍</div>
<strong>LUGAR</strong>
</div>
<div class="dimension-box">
<div style="font-size: 3rem;">👥</div>
<strong>PERSONA</strong>
</div>
</div>

*Analizando indicadores de impacto como la morbilidad y mortalidad, reconociendo que es uno de los procesos con mayor impacto en la población derechohabiente del IMSS.*

## 🧭 Estructura del Dashboard

<div class="two-columns">
<div>
### 📊 Métricas Principales
- Resumen ejecutivo de indicadores
- Análisis temporal de tendencias
- Distribución geográfica (mapas)
- Análisis demográfico por edad y sexo
</div>
<div>
### 🔗 Proceso DM y su Impacto
- Evolución temporal de indicadores
- Análisis de correlaciones
- Comparaciones entre unidades
- Módulo de pronósticos avanzados
</div>
</div>

## 📋 Métricas e Indicadores (1/2)

<div class="metric-grid">
<div class="metric-card">
**➕ Incidencia**  
*Casos nuevos de diabetes diagnosticados*  
**Fórmula:** (Casos nuevos / Población en riesgo) × 100,000  
**Fuente:** Morbi_Diabetes
</div>
<div class="metric-card">
**📊 Prevalencia**  
*% de derechohabientes con diagnóstico de DM*  
**Fórmula:** (Pacientes DM / Población Adscrita a MF) × 100  
**Fuente:** tb_censo_DM
</div>
<div class="metric-card">
**🩺 Consultas**  
*Consultas de medicina familiar por diabetes*  
**Fórmula:** (Total consultas / Población total) × 1,000  
**Fuente:** tb_consulta_dm
</div>
<div class="metric-card">
**🏥 Hospitalizaciones**  
*Egresos hospitalarios por diabetes*  
**Fórmula:** (Egresos DM / Población total) × 100,000  
**Fuente:** tb_egreso_dm
</div>
</div>

## 📋 Métricas e Indicadores (2/2)

<div class="metric-grid">
<div class="metric-card">
**💀 Mortalidad**  
*Defunciones por diabetes como causa básica*  
**Fórmula:** (Defunciones / Población total) × 100,000  
**Fuente:** Morta_Diabetes
</div>
<div class="metric-card">
**⏱️ Días de Estancia Promedio**  
*Promedio de días de estancia hospitalaria*  
**Fórmula:** mean(DiasEstancia_DM_Adsc)  
**Fuente:** tb_egreso_dm
</div>
<div class="metric-card">
**🛏️ Incapacidades**  
*Días de incapacidad temporal por cada 100 trabajadores*  
**Fórmula:** (Días incapacidad / Población RT) × 100  
**Fuente:** tb_dm_incap
</div>
<div class="metric-card">
**📅 Días por Incapacidad Promedio**  
*Promedio de duración en días por incapacidad*  
**Fórmula:** (Total NDIAS / Total FREC)  
**Fuente:** tb_dm_incap
</div>
</div>

## 🔗 Análisis Avanzado

<div class="two-columns">
<div>
### 📈 Evolución Temporal
- Comparación entre Nacional, OOAD y Unidad Médica
- Visualización interactiva con dygraphs
- Datos mensuales de múltiples años

### ⚖️ División de Indicadores
**Productividad (Eje X):** DM 01, DM 02, DM 04, DM 05, DM 06  
**Impacto (Eje Y):** DM 03, DM 07, DM 08, DM 09
</div>
<div>
### 🎯 Análisis de Cuadrantes
- **Líneas Rojas:** Valores de referencia oficiales
- **Líneas Grises:** Valores promedio
- **Cuadrante Meta:** Zona verde óptima

### 📊 Estadística de Correlación
- **Fuerte:** |r| ≥ 0.7
- **Moderada:** 0.4 ≤ |r| < 0.7
- **Débil:** 0.2 ≤ |r| < 0.4
- **Muy Débil:** |r| < 0.2
</div>
</div>

## 📈 Módulo de Pronósticos

### 🤖 Selección Automática Inteligente
El sistema compara automáticamente métodos ETS y ARIMA, seleccionando el mejor basado en criterios estadísticos (AIC).

<div class="metric-grid">
<div class="metric-card">
**🤖 Automático**  
Selección óptima por AIC comparando ETS vs ARIMA automáticamente
</div>
<div class="metric-card">
**📈 ETS**  
Suavizado exponencial con detección de Error, Tendencia y Estacionalidad
</div>
<div class="metric-card">
**📊 ARIMA**  
Modelos autoregresivos integrados de media móvil
</div>
<div class="metric-card">
**🎯 Naive**  
Modelo base de comparación (Random Walk Forecast)
</div>
</div>

## 📊 Granularidad Temporal Dual

| Aspecto | Datos Anuales | Datos Mensuales |
|---------|---------------|-----------------|
| **Puntos de datos** | ~11 | ~132+ |
| **Detección estacional** | <span class="status-no">❌ No</span> | <span class="status-yes">✅ Sí</span> |
| **Precisión del modelo** | Básica | Alta |
| **Horizonte recomendado** | 2-6 años | 6-24 meses |
| **Mejor para** | Tendencias | Operaciones |

## ⚙️ Especificaciones Técnicas

<div class="metric-grid">
<div class="metric-card">
**💾 Base de Datos**  
- SQL Server con conexión ODBC
- Procesamiento de 1M+ registros
- 7+ tablas principales de datos
</div>
<div class="metric-card">
**📊 Visualizaciones**  
- Gráficos interactivos con Plotly y Echarts4r
- Mapas geográficos con Leaflet
- Series temporales con Dygraphs
</div>
<div class="metric-card">
**🧮 Análisis Estadístico**  
- Modelos de pronóstico ARIMA y ETS
- Análisis de correlaciones
- Intervalos de confianza
</div>
<div class="metric-card">
**🗺️ Análisis Geoespacial**  
- Shapefiles de municipios mexicanos
- Visualización por OOAD
- Mapas de calor epidemiológicos
</div>
</div>

## ⚡ Rendimiento y Arquitectura

<div class="two-columns">
<div>
### 🚀 Especificaciones
- **Tiempo de carga inicial:** 5-8 minutos
- **Registros procesados:** 1M+ filas
- **Memoria recomendada:** 4GB+ RAM
- **Red:** Conexión a red IMSS interna

### 📋 Dependencias
- **R:** 4.5.0+ requerido
- **Interfaz:** Shiny & bslib
- **Base de datos:** DBI & ODBC
- **Análisis:** dplyr, forecast
</div>
<div>
### 🏗️ Arquitectura Modular

```
├── global.R          # Configuración y conexión DB
├── server.R          # Lógica del servidor
├── ui.R              # Interfaz de usuario
├── modules/          # Módulos especializados
│   ├── inicio_portada.R
│   ├── metric_summary.R
│   ├── time_chart.R
│   ├── map_chart.R
│   ├── age_sex_graph.R
│   ├── indicadores_cruce.R
│   └── simple_forecasting.R
├── data/             # Datos estáticos
└── docs/             # Documentación
```
</div>
</div>

## {.hero-slide}

# ¡Gracias!

## Dashboard desarrollado para el análisis integral del comportamiento epidemiológico de la diabetes

<br>

**Tecnologías:** R + Shiny | **Base de Datos:** SQL Server  
**Visualización:** Plotly, Leaflet, Echarts4r

<br>

*CIIMSS - Centro de Inteligencia en Salud del IMSS*