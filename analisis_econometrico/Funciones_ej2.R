# ------------------------------------------------------------
# Función para calcular RMSE
# ------------------------------------------------------------
#raiz cuadrada del promedio de errores elevados al cuadrado

rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2))
}
