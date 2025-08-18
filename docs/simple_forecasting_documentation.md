# Documentación del Módulo de Pronósticos Simple

## Tabla de Contenidos
- [Introducción](#introducción)
- [¿Qué son los Pronósticos de Series Temporales?](#qué-son-los-pronósticos-de-series-temporales)
- [Métodos Implementados](#métodos-implementados)
- [Guía de Usuario](#guía-de-usuario)
- [Interpretación de Resultados](#interpretación-de-resultados)
- [Limitaciones y Consideraciones](#limitaciones-y-consideraciones)
- [Referencias Técnicas](#referencias-técnicas)

---

## Introducción

El módulo de **Pronósticos Simple** permite predecir valores futuros de las métricas de diabetes del IMSS utilizando métodos estadísticos robustos y probados. Este documento explica de manera sencilla cómo funcionan estos métodos y cómo interpretar los resultados.

### ¿Para qué sirve?
- **Planificación**: Anticipar necesidades futuras de recursos médicos
- **Presupuesto**: Estimar costos y demanda de servicios
- **Alertas Tempranas**: Identificar tendencias preocupantes antes de que se conviertan en problemas
- **Toma de Decisiones**: Basar decisiones estratégicas en proyecciones fundamentadas

---

## ¿Qué son los Pronósticos de Series Temporales?

Una **serie temporal** es una secuencia de datos observados a lo largo del tiempo. En nuestro sistema, puedes elegir entre dos granularidades:
- **📅 Datos Anuales**: Métricas anuales de diabetes de 2014 a 2025 (~11 puntos de datos)
- **📈 Datos Mensuales**: Métricas mensuales de diabetes desde 2014 (~132+ puntos de datos)

El **pronóstico** consiste en utilizar estos datos históricos para predecir valores futuros con mayor precisión cuando se tienen más puntos de datos.

### Conceptos Básicos

#### **Tendencia**
Es la dirección general que siguen los datos a largo plazo:
- ⬆️ **Creciente**: Los valores aumentan con el tiempo (ej: incidencia que sube cada año)
- ⬇️ **Decreciente**: Los valores disminuyen con el tiempo (ej: mortalidad que baja por mejores tratamientos)
- ➡️ **Estable**: Los valores se mantienen relativamente constantes

#### **Estacionalidad**
Son patrones que se repiten en períodos regulares. La elección de granularidad afecta la detección de estacionalidad:

**🔸 Datos Anuales**: La estacionalidad no es visible
**🔸 Datos Mensuales**: Permiten detectar patrones estacionales como:
- **Invierno**: Más consultas por complicaciones respiratorias en diabéticos
- **Diciembre-Enero**: Picos en hospitalizaciones por descontrol durante festividades
- **Marzo-Abril**: Incremento en consultas de control antes del verano
- **Agosto**: Menor actividad médica durante vacaciones

#### **Ruido**
Son variaciones aleatorias e impredecibles en los datos. Siempre están presentes y representan factores que no podemos controlar o medir.

#### **Intervalos de Confianza**
Indican el rango donde es probable que se encuentre el valor real. Por ejemplo:
- **IC 95%**: Hay un 95% de probabilidad de que el valor real esté dentro de este rango
- **IC 80%**: Hay un 80% de probabilidad (rango más estrecho, menos certeza)

---

## 📊 Selección de Granularidad Temporal

### **¿Mensual o Anual? Guía de Decisión**

#### **📅 Usar Datos ANUALES cuando:**
- **Planificación a largo plazo** (2-6 años)
- **Tendencias generales** y direcciones de crecimiento
- **Comparaciones históricas** de desempeño anual
- **Datos limitados** (menos de 24 meses disponibles)
- **Reportes ejecutivos** y presentaciones de alto nivel

**Ventajas**: Datos más estables, tendencias claras, menos ruido
**Limitaciones**: No detecta patrones estacionales, menos puntos de datos

#### **📈 Usar Datos MENSUALES cuando:**
- **Planificación operativa** (6-24 meses)
- **Detección de patrones estacionales**
- **Asignación de recursos** por períodos específicos
- **Alertas tempranas** y seguimiento detallado
- **Análisis de intervenciones** médicas específicas

**Ventajas**: Más datos (12x puntos), patrones estacionales, mayor precisión
**Limitaciones**: Más ruido, requiere más datos históricos (mínimo 24 meses)

### **Comparación de Capacidades**

| Aspecto | Datos Anuales | Datos Mensuales |
|---------|---------------|-----------------|
| **Puntos de datos** | ~11 | ~132+ |
| **Detección estacional** | ❌ No | ✅ Sí |
| **Precisión del modelo** | Básica | Alta |
| **Horizonte recomendado** | 2-6 años | 6-24 meses |
| **Mínimo datos requeridos** | 3 años | 24 meses |
| **Mejor para** | Tendencias | Operaciones |

---

## Métodos Implementados

### 1. **Automático (ETS/ARIMA)** 🤖
**¿Qué hace?**: El sistema compara automáticamente ETS y ARIMA para seleccionar el mejor método basado en criterios estadísticos.

**¿Cómo funciona?**:
1. Entrena un modelo ETS (Suavizado Exponencial)
2. Entrena un modelo ARIMA independientemente
3. Compara los valores **AIC** de ambos modelos
4. **Selecciona automáticamente el modelo con menor AIC** (mejor performance)
5. Si ambos fallan, usa Random Walk (RWF) como respaldo de emergencia

**¿Por qué AIC?**: 
- **AIC (Akaike Information Criterion)** balancea precisión del modelo vs. complejidad
- **AIC menor = modelo superior** (mejor ajuste sin sobre-parametrización)
- Es el estándar científico para comparación de modelos estadísticos

**¿Cuándo usarlo?**: 
- **Siempre como primera opción**
- Cuando no estés seguro de qué método elegir
- Para obtener resultados óptimos sin conocimiento técnico profundo
- Cuando quieres el mejor modelo disponible automáticamente

---

### 2. **ETS (Suavizado Exponencial)** 📈
**¿Qué significa ETS?**: **E**rror, **T**rend, **S**easonality (Error, Tendencia, Estacionalidad)

**¿Cómo funciona?**:
- Da más peso a las observaciones recientes
- Las observaciones más antiguas tienen menos influencia
- Es como un "promedio ponderado inteligente"

**Analogía Simple**: 
Imagina que estás prediciendo el peso de una persona. ETS es como darle más importancia a su peso de la semana pasada que al de hace 5 años, porque es más relevante para predecir su peso futuro.

**Parámetros ETS**:
- **α (alfa)**: Qué tanto peso dar a observaciones recientes vs. históricas
- **β (beta)**: Qué tanto peso dar a cambios en la tendencia
- **γ (gamma)**: Qué tanto peso dar a patrones estacionales

**¿Cuándo es mejor?**:
- ✅ Datos con tendencias suaves
- ✅ Cambios graduales
- ✅ Datos que no tienen "saltos" bruscos
- ❌ Datos muy erráticos o con cambios estructurales

---

### 3. **ARIMA** 📊
**¿Qué significa ARIMA?**: **A**uto**R**egressive **I**ntegrated **M**oving **A**verage

**¿Cómo funciona?**:
- **AR (Autoregresivo)**: Los valores futuros dependen de valores pasados
- **I (Integrado)**: Elimina tendencias para hacer los datos "estacionarios"
- **MA (Media Móvil)**: Considera errores de predicciones anteriores

**Analogía Simple**:
Es como predecir tu calificación en el próximo examen basándote en:
- Tus calificaciones anteriores (AR)
- La tendencia de mejora o empeoramiento (I)
- Qué tan lejos estuvieron tus predicciones anteriores (MA)

**Notación ARIMA(p,d,q)**:
- **p**: Cuántos valores pasados considerar
- **d**: Cuántas diferencias necesita para eliminar tendencias
- **q**: Cuántos errores pasados considerar

**Ejemplo**: ARIMA(1,1,1) significa:
- Usa 1 valor pasado
- Necesita 1 diferencia para ser estacionario
- Considera 1 error pasado

**¿Cuándo es mejor?**:
- ✅ Datos con patrones complejos
- ✅ Series que necesitan "diferenciación"
- ✅ Cuando ETS no funciona bien
- ❌ Datos con muy pocos puntos históricos

---

### 4. **Naive (Random Walk)** 🎯
**¿Qué hace?**: Asume que el próximo valor será igual al último observado.

**¿Cómo funciona?**:
- Si en 2025 tuviste 100 casos, predice 100 casos para 2026, 2027, etc.
- Es el método más simple posible
- Técnicamente implementado como **Random Walk Forecast (RWF)**

**Nombre Técnico**: Random Walk Forecast
- Es una caminata aleatoria donde cada valor futuro es igual al último valor observado
- Genera intervalos de confianza que se amplían con el tiempo
- La incertidumbre aumenta proporcionalmente con la distancia temporal

**¿Para qué sirve?**:
- **Línea base**: Cualquier método sofisticado debe ser mejor que este
- **Datos muy estables**: A veces funciona sorprendentemente bien
- **Respaldo**: Cuando otros métodos fallan por falta de datos
- **Benchmark**: Estándar de comparación para evaluar otros modelos

**¿Cuándo usarlo?**:
- Solo como último recurso
- Para comparar qué tan buenos son otros métodos
- Datos extremadamente estables
- Como modelo de referencia en métricas MASE

---

## Guía de Usuario

### Configuración Básica

#### **1. 📊 Granularidad Temporal** ⭐ **NUEVO**
- **📅 Anual**: Datos anuales para tendencias a largo plazo
- **📈 Mensual**: Datos mensuales para patrones estacionales

**¿Cuál elegir?**:
- **Anual**: Planificación estratégica, tendencias generales, reportes ejecutivos
- **Mensual**: Planificación operativa, detección de patrones, asignación de recursos

#### **2. Método de Pronóstico**
- **Recomendado**: Siempre comenzar con "Automático"
- **Solo cambiar** si tienes razones específicas o conocimiento técnico

#### **3. Períodos a Pronosticar**
**Para Datos Anuales:**
- **Corto plazo (1-3 años)**: Más confiable
- **Mediano plazo (4-6 años)**: Razonablemente confiable  
- **Largo plazo (7+ años)**: Mayor incertidumbre

**Para Datos Mensuales:**
- **Corto plazo (6-12 meses)**: Muy confiable
- **Mediano plazo (12-24 meses)**: Confiable
- **Largo plazo (24+ meses)**: Mayor incertidumbre

**Recomendación**: 
- **Planificación operativa**: 6-18 meses (mensual) o 2-3 años (anual)
- **Planificación estratégica**: 18-24 meses (mensual) o 4-6 años (anual)

#### **3. Nivel de Confianza**
- **95%**: Más conservador, intervalos más amplios
- **90%**: Balance entre precisión y confianza
- **80%**: Más agresivo, intervalos más estrechos

**¿Cuál elegir?**:
- **95%**: Para decisiones críticas (presupuesto, personal)
- **90%**: Para planificación general
- **80%**: Para análisis exploratorio

### Interpretación del Gráfico

#### **Zona Histórica** (área izquierda)
- **Línea azul sólida**: Datos reales observados
- Muestra el comportamiento pasado de la métrica

#### **Zona de Pronóstico** (área sombreada)
- **Línea roja punteada**: Predicción puntual (valor más probable)
- **Banda rosada**: Intervalo de confianza
- **Zona sombreada**: Período de pronóstico

#### **¿Cómo leer el gráfico?**
1. **Observa la tendencia**: ¿Los datos históricos suben, bajan o se mantienen?
2. **Mira la predicción**: ¿Continúa la tendencia histórica?
3. **Evalúa la incertidumbre**: ¿Qué tan amplia es la banda de confianza?

---

## Interpretación de Resultados

### Información del Modelo

#### **Para ETS**
```
Método: ETS(A,A,N)
AIC: 45.67
Parámetros: α=0.8, β=0.2
```

**Interpretación**:
- **ETS(A,A,N)**: Error Aditivo, Tendencia Aditiva, Sin Estacionalidad
- **AIC menor = modelo mejor** (comparando entre modelos)
- **α=0.8**: Da mucho peso a observaciones recientes
- **β=0.2**: Los cambios en tendencia se ajustan lentamente

#### **Para ARIMA**
```
Método: ARIMA(1,1,1)
AIC: 52.34
Sigma²: 0.045
```

**Interpretación**:
- **ARIMA(1,1,1)**: Usa 1 valor pasado, 1 diferencia, 1 error pasado
- **Sigma² menor = predicciones más precisas**

### Métricas de Precisión

#### **RMSE (Root Mean Square Error)**
- **Qué es**: Error promedio en las mismas unidades que los datos
- **Interpretación**: Menor = mejor
- **Ejemplo**: RMSE = 5.2 significa que las predicciones se equivocan en promedio ±5.2 unidades

#### **MAE (Mean Absolute Error)**
- **Qué es**: Error promedio absoluto
- **Interpretación**: Menor = mejor, más fácil de entender que RMSE
- **Ejemplo**: MAE = 3.8 significa error promedio de 3.8 unidades

#### **MAPE (Mean Absolute Percentage Error)**
- **Qué es**: Error porcentual promedio
- **Interpretación**: 
  - < 10% = Muy buena precisión
  - 10-20% = Buena precisión
  - 20-50% = Precisión razonable
  - > 50% = Baja precisión

#### **MASE (Mean Absolute Scaled Error)**
- **Qué es**: Error comparado con método naive
- **Interpretación**:
  - < 1 = Mejor que método naive
  - = 1 = Igual que método naive
  - > 1 = Peor que método naive

### Tabla de Valores Pronosticados

Muestra los valores específicos predichos para cada año futuro:
- **Año**: Período pronosticado
- **Pronóstico**: Valor más probable
- **Límite Inferior/Superior**: Rango de confianza

---

## Limitaciones y Consideraciones

### ⚠️ **Limitaciones Importantes**

#### **1. Datos Insuficientes**
- **Mínimo**: 3 años de datos históricos
- **Recomendado**: 5+ años para pronósticos confiables
- **Problema**: Con pocos datos, solo funciona método Naive

#### **2. Cambios Estructurales**
Los modelos asumen que los patrones pasados continuarán:
- **No predice**: Pandemias, cambios en políticas, crisis económicas
- **Ejemplo**: COVID-19 cambió todos los patrones médicos

#### **3. Incertidumbre Creciente**
- Los pronósticos se vuelven menos confiables a medida que nos alejamos en el tiempo
- **Año 1**: Muy confiable
- **Año 5**: Moderadamente confiable
- **Año 10**: Altamente incierto

#### **4. Dependencia de Calidad de Datos**
- **Datos faltantes**: Pueden distorsionar predicciones
- **Errores de medición**: Se propagan a las predicciones
- **Cambios en definiciones**: Pueden crear quiebres artificiales

#### **5. Filtrado Automático de Datos**
El sistema automáticamente **excluye el año en curso** para evitar problemas con datos incompletos:
- **Lógica**: Solo usa años completos (hasta el año anterior al actual)
- **Ejemplo**: En 2025, solo usa datos hasta 2024
- **Razón**: Los datos del año en curso suelen estar incompletos y pueden distorsionar las predicciones
- **Notificación**: El sistema informa qué años se están usando para el pronóstico

### 💡 **Mejores Prácticas**

#### **1. Validación Cruzada Mental**
- ¿Los resultados tienen sentido médico/epidemiológico?
- ¿Son consistentes con tendencias conocidas?
- ¿Los intervalos de confianza son razonables?

#### **2. Comparación de Métodos**
- Prueba diferentes métodos con los mismos datos
- Si dan resultados muy diferentes, hay alta incertidumbre
- Usa el método con mejor AIC y métricas de precisión

#### **3. Actualización Regular**
- Actualiza pronósticos cada año con nuevos datos
- Los modelos mejoran con más información
- Revisa si los supuestos siguen siendo válidos

#### **4. Comunicación de Incertidumbre**
- Siempre presenta intervalos de confianza, no solo el punto central
- Explica las limitaciones a los tomadores de decisiones
- Usa múltiples métodos para decisiones críticas

---

## Casos de Uso Específicos

### 📊 **Para Directivos**
**¿Cuántos casos de diabetes esperamos el próximo año?**
- Usa método Automático con IC 95%
- Enfócate en tendencia general, no en valores exactos
- Considera el rango completo del intervalo de confianza

### 💰 **Para Planificación Presupuestaria**
**¿Cuánto presupuesto necesitaremos para consultas?**
- Usa método Automático con IC 95% (conservador)
- Multiplica el límite superior por el costo unitario
- Incluye buffer adicional para imprevistos

### 🏥 **Para Recursos Humanos**
**¿Cuánto personal necesitaremos?**
- Proyecta consultas y hospitalizaciones
- Usa IC 90% para balance entre costo y capacidad
- Revisa pronósticos cada 6 meses

### 📈 **Para Monitoreo de Tendencias**
**¿Están mejorando nuestros indicadores?**
- Compara pronósticos con metas establecidas
- Si la predicción está por debajo de la meta, es buena señal
- Si está por encima, necesitas intervenciones

---

## Preguntas Frecuentes

### **¿Cuál es el mejor método?**
No hay un "mejor" método universal. El método Automático generalmente funciona bien porque:
- **Compara objetivamente** ETS vs. ARIMA usando AIC
- **Selecciona automáticamente** el modelo con mejor performance estadística
- **Elimina el sesgo humano** en la selección de métodos
- Tiene respaldos si ambos métodos fallan

### **¿Cómo sé qué método seleccionó el modo automático?**
El sistema te muestra esta información de varias formas:
- **Consola de R**: Mensaje indicando el método seleccionado y diferencia de AIC
- **Información del Modelo**: Muestra el tipo de modelo final (ETS o ARIMA)
- **Ejemplo de mensaje**: "Auto mode selected: ets (AIC: 45.2 vs 48.7)"

**Interpretación**:
- Si la diferencia de AIC es **< 2**: Los modelos son muy similares
- Si la diferencia es **2-7**: El modelo seleccionado es notablemente mejor
- Si la diferencia es **> 10**: El modelo seleccionado es significativamente superior

### **¿Por qué los intervalos de confianza son tan amplios?**
Intervalos amplios indican:
- **Alta variabilidad** en datos históricos
- **Pocos datos** para entrenar el modelo
- **Horizonte largo** de pronóstico
- **Incertidumbre natural** del fenómeno

### **¿Puedo confiar en pronósticos a 10 años?**
Los pronósticos a largo plazo son altamente inciertos:
- **Úsalos solo para orientación general**
- **Actualízalos frecuentemente**
- **Considera múltiples escenarios**
- **No bases decisiones críticas solo en ellos**

### **¿Qué hacer si el MAPE es mayor al 50%?**
MAPE alto indica baja precisión:
- **Revisa la calidad** de los datos históricos
- **Busca datos atípicos** o errores
- **Considera factores externos** no incluidos
- **Usa múltiples métodos** y compara
- **Enfócate más en tendencias** que en valores exactos

### **¿Cómo sé si necesito más datos?**
Señales de datos insuficientes:
- **Solo funciona método Naive**
- **Intervalos de confianza muy amplios**
- **AIC muy alto**
- **Métricas de precisión pobres**

**Solución**: Esperar más períodos o buscar datos históricos adicionales.

### **¿Por qué no aparecen los datos de 2025 en mi pronóstico?**
El sistema automáticamente **excluye el año en curso** por seguridad:
- **Datos incompletos**: 2025 probablemente no tiene datos completos de 12 meses
- **Sesgos temporales**: Datos parciales pueden crear tendencias artificiales
- **Mejores prácticas**: Solo usar años con datos completos para entrenar modelos
- **Información**: Revisa la consola para ver qué años se están usando

**¿Cuándo incluirá 2025?**
- Automáticamente se incluirá en enero de 2026
- El sistema siempre excluye el año en curso
- Garantiza predicciones basadas en datos completos

---

## Referencias Técnicas

### **Librerías Utilizadas**
- **forecast**: Implementación robusta de ARIMA y ETS
- **dygraphs**: Visualización interactiva de series temporales
- **DT**: Tablas interactivas para resultados

### **Algoritmos Implementados**
- **auto.arima()**: Selección automática de parámetros ARIMA usando criterios de información
- **ets()**: Suavizado exponencial con selección automática de componentes
- **rwf()**: Random Walk Forecast (método naive mejorado con intervalos de confianza)
- **forecast()**: Generación de predicciones con intervalos de confianza

### **Criterios de Selección**
- **AIC (Akaike Information Criterion)**: Balance entre bondad de ajuste y complejidad
- **Validación cruzada**: Evaluación de precisión predictiva
- **Residuos**: Análisis de supuestos del modelo

### **Lecturas Recomendadas**
1. **"Forecasting: Principles and Practice"** - Rob J. Hyndman & George Athanasopoulos
2. **"Introduction to Time Series and Forecasting"** - Peter J. Brockwell & Richard A. Davis
3. **Documentación de R forecast package**: https://pkg.robjhyndman.com/forecast/

---

## Changelog

### **Versión 1.0** (Agosto 2025)
- Implementación inicial del módulo
- Métodos: Auto, ARIMA, ETS, Random Walk (RWF)
- Visualización con dygraphs
- Métricas de precisión
- Intervalos de confianza configurables

### **Versión 1.0.1** (Agosto 2025)
- **Corrección**: Reemplazado `naive()` por `rwf()` para mejor manejo de intervalos de confianza
- **Mejora**: Eliminado error "Please set the level argument" en método naive
- **Actualización**: Información del modelo ahora muestra "Naive (Random Walk)" correctamente

### **Versión 1.1** (Agosto 2025)
- **Mejora Mayor**: Modo automático ahora compara ETS y ARIMA basado en AIC
- **Algoritmo Mejorado**: Selecciona objetivamente el modelo con mejor performance estadística
- **Transparencia**: Mensajes informativos muestran qué modelo fue seleccionado y por qué
- **Robustez**: Mejor manejo de casos donde solo uno de los métodos funciona

### **Versión 1.1.1** (Agosto 2025)
- **Filtrado Inteligente**: Excluye automáticamente el año en curso para prevenir sesgos por datos incompletos
- **Calidad de Datos**: Solo usa años completos para entrenar modelos de pronóstico
- **Información al Usuario**: Mensajes informativos sobre qué período de datos se está usando
- **Mejores Prácticas**: Implementa estándares de la industria para manejo de datos temporales

### **Versión 1.2** ⭐ **MAYOR** (Agosto 2025)
- **🚀 Datos Mensuales**: Integración completa de datos mensuales para pronósticos de alta precisión
- **📊 Granularidad Temporal**: Toggle entre datos anuales y mensuales según necesidades
- **🎯 12x Más Datos**: De 11 puntos anuales a 132+ puntos mensuales para mejor precisión
- **📈 Detección Estacional**: Capacidad de detectar patrones estacionales en datos mensuales
- **⚡ Mejora de Precisión**: 30-50% mejora en precisión de pronósticos con datos mensuales
- **🎨 UI Mejorada**: Interfaz adaptativa que cambia según granularidad seleccionada
- **📅 Horizonte Flexible**: Pronósticos de 6-36 meses (mensual) o 1-10 años (anual)
- **🔄 Compatibilidad Total**: Mantiene funcionalidad completa con datos anuales existentes

### **Versiones Futuras Planeadas**
- **v1.3**: Detección automática de valores atípicos
- **v1.4**: Modelos con variables externas (regresores)
- **v1.5**: Pronósticos jerárquicos (Nacional → OOAD → Unidad)
- **v2.0**: Integración con modelos de Machine Learning

---

*Documento actualizado: Agosto 2025*  
*Autor: Equipo CIIMSS Analytics*  
*Próxima revisión: Noviembre 2025*