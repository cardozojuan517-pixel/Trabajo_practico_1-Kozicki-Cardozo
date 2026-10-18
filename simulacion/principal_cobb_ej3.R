
# =========================================================
# SCRIPT PRINCIPAL: SIMULACIÓN COBB-DOUGLAS
# =========================================================

# Limpiar entorno, consola y gráficos
rm(list = ls())  # Borra variables del Environment
cat("\014")      # Limpia la consola
graphics.off()   # Cierra y borra los gráficos

# Reproducibilidad
set.seed(345)

# Cargar librerías
library(tidyverse)

# Cargar funciones auxiliares
source("funciones_cobb.R")

# ---------------------------------------------------------
# Inciso 3: Simulación prueba
# ---------------------------------------------------------
N <- 10000
K <- 20      # Grados de libertad 
p1_prueba <- 5         # Precio bien 1
p2_prueba <- 7         # Precio bien 2
a1_prueba <- 0.3       # Preferencia x1
a2_prueba <- 0.7       # Preferencia x2

# Generar ingresos
ingresos <- simular_ingreso(N, k = K)

# Calculemos demandas prueba
simu_prueba <- demanda_cd(
  Y = ingresos, 
  p1 = p1_prueba, 
  p2 = p2_prueba, 
  A1 = a1_prueba, 
  A2 = a2_prueba
)

# Medias y Cuartiles
summary(simu_prueba)

################################################################
# Gráficos de densidades

# 1. Densidad para la Demanda del Bien 1 
g_x1 <- ggplot(simu_prueba, aes(x = x1_opt)) +
  geom_density(fill = "steelblue", alpha = 0.6, color = "darkblue") +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "lightblue", alpha = 0.3, color = "white") +
  labs(
    title = expression("Distribución Empírica de " ~ x[1]^"*"),
    x = "Cantidad Demandada Bien 1",
    y = "Densidad"
  ) +
  theme_minimal()

# 2. Densidad para la Demanda del Bien 2 
g_x2 <- ggplot(simu_prueba, aes(x = x2_opt)) +
  geom_density(fill = "forestgreen", alpha = 0.6, color = "darkgreen") +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "lightgreen", alpha = 0.3, color = "white") +
  labs(
    title = expression("Distribución Empírica de " ~ x[2]^"*"),
    x = "Cantidad Demandada Bien 2",
    y = "Densidad"
  ) +
  theme_minimal()

# 3. Densidad para la Utilidad Indirecta 
g_u <- ggplot(simu_prueba, aes(x = u_opt)) +
  geom_density(fill = "purple", alpha = 0.6, color = "purple4") +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "plum", alpha = 0.3, color = "white") +
  labs(
    title = expression("Distribución Empírica de " ~ U^"*"),
    x = "Nivel de Utilidad Indirecta",
    y = "Densidad"
  ) +
  theme_minimal()

# Mostrar los 3 gráficos uno a uno:
print(g_x1)
print(g_x2)
print(g_u)



# ---------------------------------------------------------
# Inciso 4: Probabilidad de bajo consumo (ejemplo c = 2)
# ---------------------------------------------------------
prob_x1_bajo <- prob_bajo_consumo(simu_prueba$x1_opt, umbral_c = 2)
cat("Probabilidad de que x1 < 2 (prueba):", prob_x1_bajo, "\n")

# ---------------------------------------------------------
# Incisos 5 y 6: Shock de Precios (+20% en p1)
# ---------------------------------------------------------
p1_shock <- p1_prueba * 1.2

simu_shock <- demanda_cd(
  Y = ingresos, 
  p1 = p1_shock, 
  p2 = p2_prueba, 
  A1 = a1_prueba, 
  A2 = a2_prueba
)

# Unir datos para graficar comparación
comparacion_shock <- bind_rows(
  simu_prueba %>% mutate(Escenario = "Pre-Shock (p1 = 5)"),
  simu_shock %>% mutate(Escenario = "Post-Shock (p1 = 6)")
)

# Gráfico comparativo de densidades para x1*
ggplot(comparacion_shock, aes(x = x1_opt, fill = Escenario)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Efecto del Shock de Precios en el Consumo del Bien 1",
    x = "Demanda óptima (x1*)",
    y = "Densidad"
  ) +
  theme_minimal()

# Comparación de la utilidad promedio U* y de la masa de bajo consumo
resumen_shock <- comparacion_shock %>%
  group_by(Escenario) %>%
  summarise(
    x1_promedio = mean(x1_opt),
    U_promedio  = mean(u_opt),
    pct_bajo_2  = mean(x1_opt < 2) * 100  # Porcentaje de hogares con x1 < 2
  )

print(resumen_shock)

# ---------------------------------------------------------
# Inciso 7: heterogeneidad en preferencias
# ---------------------------------------------------------
# A1 sigue una distribución Beta(a = 2, b = 3)
A1_hetero <- rbeta(N, shape1 = 2, shape2 = 3)
A2_hetero <- 1 - A1_hetero

# ---------------------------------------------------------
# Inciso 3: con heterogeneidad
# ---------------------------------------------------------
simu_hetero_prueba <- demanda_cd(
  Y = ingresos, 
  p1 = p1_prueba, 
  p2 = p2_prueba, 
  A1 = A1_hetero, 
  A2 = A2_hetero
)

# Medias y Cuartiles (Repetición Inciso 3 con Heterogeneidad)
cat("--- Resumen Estadístico con Heterogeneidad (Pre-shock) ---\n")
summary(simu_hetero_prueba)

# ---------------------------------------------------------
# Inciso 4: con heterogeneidad
# ---------------------------------------------------------
prob_bajo_hetero_prueba <- prob_bajo_consumo(simu_hetero_prueba$x1_opt, umbral_c = 2)
cat("Probabilidad x1 < 2 con heterogeneidad (Pre-Shock):", prob_bajo_hetero_prueba, "\n")

# ---------------------------------------------------------
# Incisos 5 y 6: con heterogeneidad
# ---------------------------------------------------------

simu_hetero_shock <- demanda_cd(
  Y = ingresos, 
  p1 = p1_shock, 
  p2 = p2_prueba, 
  A1 = A1_hetero, 
  A2 = A2_hetero
)

#Gráfico Comparativo del Shock (Repetición Inciso 6)

comparacion_hetero_shock <- bind_rows(
  simu_hetero_prueba %>% mutate(Escenario = "Pre-Shock Heterogéneo (p1 = 5)"),
  simu_hetero_shock  %>% mutate(Escenario = "Post-Shock Heterogéneo (p1 = 6)")
)

# Gráfico de densidades superpuestas para x1* con heterogeneidad
g_hetero_shock <- ggplot(comparacion_hetero_shock, aes(x = x1_opt, fill = Escenario)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Shock de Precios con Heterogeneidad en Preferencias",
    x = expression("Demanda Óptima (" ~ x[1]^"*" ~ ")"),
    y = "Densidad"
  ) +
  theme_minimal()

print(g_hetero_shock)


# Resumen Numérico del Shock 

resumen_hetero_shock <- comparacion_hetero_shock %>%
  group_by(Escenario) %>%
  summarise(
    x1_promedio = mean(x1_opt),
    U_promedio  = mean(u_opt),
    pct_bajo_2  = mean(x1_opt < 2) * 100
  )

print(resumen_hetero_shock)






