

# Funciones: COBB-DOUGLAS

# 1. Generación de ingresos con distribución Chi-cuadrado
simular_ingreso <- function(n, k) {
  # k representa los grados de libertad.
  # A mayor k, mayor es el ingreso promedio (E[Y] = k) y la varianza (Var(Y) = 2k).
  rchisq(n, df = k)
}

# 2. Demanda óptima Cobb-Douglas y utilidad 
demanda_cd <- function(Y, p1, p2, A1, A2) {
  x1_opt <- (A1 * Y) / p1
  x2_opt <- (A2 * Y) / p2
  u_opt <- (x1_opt^A1) * (x2_opt^A2)
  
  tibble(
    x1_opt = x1_opt,
    x2_opt = x2_opt,
    u_opt = u_opt
  )
}

# 4. Probabilidad de bajo consumo en un bien
prob_bajo_consumo <- function(demanda_vector, umbral_c) {
  mean(demanda_vector < umbral_c)
}

