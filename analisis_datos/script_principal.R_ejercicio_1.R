

# =========================================================
# SCRIPT PRINCIPAL: ANÁLISIS DE DATOS - SOJA ARGENTINA
# =========================================================

#Limpiar:
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

# -- Reproducibilidad ---
set.seed(345)

# Cargar librerías necesarias

library(readxl)
library(tidyverse)
library(readr)


#--EJERCICIO 1 DE ANALSISI DE DATOS--

datos_soja <- read.csv("C:/Users/Usuario/Desktop/analisis_datos/soja-serie-1941-2024-anual.csv")
View(datos_soja)

#me fijo que no me falten datos (si son 0 entocnes no hay vacias): 

colSums(is.na(datos_soja))
summary(datos_soja)

##########################################################################################

# PREGUNTA 1: ¿Cómo evolucionó la producción total y el rendimiento de la soja?

# Años de mayor y menor producción
max_prod <- datos_soja %>% 
  arrange(desc(produccion_soja_t)) %>% 
  slice(1)

min_prod <- datos_soja %>% 
  arrange(produccion_soja_t) %>% 
  slice(1)

# Gráfico de evolución del rendimiento (kg/ha)
ggplot(datos_soja, aes(x = indice_tiempo, y = rendimiento_soja_kgxha)) +
  geom_line(color = "forestgreen", linewidth = 1) +
  labs(
    title = "Evolución del rendimiento de Soja en Argentina (1941-2024)",
    x = "Año",
    y = "Rendimiento (kg/ha)"
  ) +
  theme_minimal()

# PREGUTNA 2: ¿Qué porcentaje de la superficie sembrada se logra cosechar cada año?

# Columna del porcentaje cosechado
datos_soja <- datos_soja %>% 
  mutate(pct_cosechado = (superficie_cosechada_soja_ha / superficie_sembrada_soja_ha) * 100)

# Promedio histórico y años críticos (< 80% cosechado)
promedio_historia <- datos_soja %>% 
  summarise(promedio_pct = mean(pct_cosechado, na.rm = TRUE))

anios_criticos <- datos_soja %>% 
  filter(pct_cosechado < 80) %>% 
  select(indice_tiempo, pct_cosechado)


#PREGUNTA 3: ¿Existe una aceleración del rendimiento a partir del año 1996 (era de la soja transgénica)?

# Separar por perido pre y post
pre_transgenica <- datos_soja %>% 
  filter(indice_tiempo < 1996)

post_transgenica <- datos_soja %>% 
  filter(indice_tiempo >= 1996)

# Calcular los promedios de cada tabla por separado
media_pre_rendimiento <- mean(pre_transgenica$rendimiento_soja_kgxha, na.rm = TRUE)
media_post_rendimiento <- mean(post_transgenica$rendimiento_soja_kgxha, na.rm = TRUE)

media_pre_produccion <- mean(pre_transgenica$produccion_soja_t, na.rm = TRUE)
media_post_produccion <- mean(post_transgenica$produccion_soja_t, na.rm = TRUE)

# Armar el cuadro comparativo manualmente con los resultados obtenidos

tabla <- data.frame(
  periodo = c("Post-1996", "Pre-1996"),
  promedio_rendimiento = c(media_post_rendimiento, media_pre_rendimiento),
  promedio_produccion = c(media_post_produccion, media_pre_produccion)
)

view(tabla)

# Porcentaje de aumento en el rendimiento por hectárea
var_pct_rendimiento <- ((media_post_rendimiento / media_pre_rendimiento) - 1) * 100

# Porcentaje de aumento en la producción total
var_pct_produccion <- ((media_post_produccion / media_pre_produccion) - 1) * 100

# Mostrar en la consola con texto explicativo
cat("Aumento en rendimiento (kg/ha):", round(var_pct_rendimiento, 2), "%\n")
cat("Aumento en producción total (t):", round(var_pct_produccion, 2), "%\n")










