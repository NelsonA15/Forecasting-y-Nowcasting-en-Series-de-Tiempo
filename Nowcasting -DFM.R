################################################################################
#
#                            PROYECTO FINAL. SERIES DE TIEMPO
#                                        CIMAT
#                                NELSON ARIZA MORALES 
#                             ESTIMACION DEL PIB TRIMESTRAL
#                                      NOWCASTING
#
################################################################################
# Cargar librerías necesarias
library(stats)
library(vars)
library(ggplot2)
library(reshape2)
library(KFAS)
library(dplyr)

# Configuración de la simulación
set.seed(42)
n_time <- 100  # Número de periodos
n_series <- 5  # Número de indicadores
r_factors <- 2 # Número de factores latentes

# Generar una tendencia lineal y componente oscilatoria
trend <- seq(1, n_time, length.out = n_time)
oscillation <- sin(seq(0, 6 * pi, length.out = n_time)) * 5 

# Generar factores 
factors <- matrix(cumsum(rnorm(n_time * r_factors)), ncol = r_factors) +
  matrix(rep(trend + oscillation, each = r_factors), ncol = r_factors)

# Matriz de cargadores factoriales (Lambda)
loadings <- matrix(runif(n_series * r_factors, 0.5, 1.5), ncol = r_factors)

# Generar series indicadores
noise <- matrix(rnorm(n_time * n_series, 0, 2), ncol = n_series)
economic_series <- factors %*% t(loadings) + noise + trend + oscillation

# Simular PIB trimestral
pib <- 2 * factors[, 1] + 1.5 * factors[, 2] + rnorm(n_time, 0, 1) + 0.5 * trend + oscillation

# Crear un DataFrame de indicadores
dates <- seq(as.Date("2000-01-01"), by = "month", length.out = n_time)
data <- data.frame(Date = dates, PIB = pib, economic_series)
colnames(data)[3:(2 + n_series)] <- c("Ventas", "Producción", "Empleo", "Comercio", "Finanzas")
head(data)

#Graficar indicadores y PIB
data_melt <- melt(data, id.vars = "Date", variable.name = "Indicador", value.name = "Valor")
data_pib <- subset(data_melt, Indicador == "PIB")
data_indicadores <- subset(data_melt, Indicador != "PIB")

# Graficar los indicadores
ggplot(data_indicadores, aes(x = Date, y = Valor, color = Indicador)) +
  geom_line() +
  labs(title = "Indicadores Económicos Simulados con Mayor Variabilidad", 
       x = "Fecha", y = "Valor", color = "Indicador") +
  theme_minimal()

# Graficar el PIB
ggplot(data_pib, aes(x = Date, y = Valor)) +
  geom_line(color = "blue") +
  labs(title = "PIB Trimestral Simulado", 
       x = "Fecha", y = "Valor") +
  theme_minimal()



################################################################
# NOWCASTING CON DINAMIC FACTOR MODEL DMF

# Normalizar
data_normalized <- scale(data[, -c(1, 2)])  # Excluir Fecha y PIB
pib <- scale(data$PIB)

n_series <- ncol(data_normalized) 
r_factors <- 2

# Configuración del modelo de estado espacio
loadings <- matrix(runif(n_series * r_factors, 0.5, 1.5), nrow = n_series, ncol = r_factors)
SSmodel <- SSModel(
  data_normalized ~ -1 +
    SSMcustom(
      Z = loadings, T = diag(r_factors), R = diag(r_factors),
      Q = diag(r_factors), P1 = diag(r_factors) * 10
    ),
  H = diag(n_series)
)

# Ajustar el modelo
fitted_model <- fitSSM(SSmodel, inits = rep(1, r_factors + n_series))
kfs_results <- KFS(fitted_model$model)

# Extraer factores
factors_est <- as.data.frame(kfs_results$alphahat)
colnames(factors_est) <- paste0("Factor_", 1:r_factors)
factors_est$Date <- data$Date

#  Factores ~ PIB
pib_model <- lm(pib ~ ., data = as.data.frame(factors_est[, -which(colnames(factors_est) == "Date")]))
summary(pib_model)

# Predicción
prediction_results <- predict(fitted_model$model, n.ahead = 1, interval = "prediction")
factors_future <- sapply(prediction_results, function(x) x[1, "fit"])
factors_future_df <- as.data.frame(t(factors_future))
colnames(factors_future_df) <- paste0("Factor_", 1:r_factors)

factor_columns <- colnames(factors_est)[grep("Factor_", colnames(factors_est))]
factors_future_df <- factors_future_df[, factor_columns, drop = FALSE]

pib_pred <- predict(pib_model, newdata = factors_future_df)

# Convertir la predicción a la escala original
pib_mean <- attr(scale(data$PIB), "scaled:center")
pib_sd <- attr(scale(data$PIB), "scaled:scale")
pib_pred_original <- (pib_pred * pib_sd/2) + pib_mean

# Calcular RMSE para evaluar el modelo
pib_pred_all <- predict(pib_model, newdata = factors_est)
pib_pred_original_all <- (pib_pred_all * pib_sd) + pib_mean
rmse <- sqrt(mean((data$PIB - pib_pred_original_all)^2))
cat("RMSE del modelo:", rmse, "\n")

# Gráficas
# 1. Indicadores Económicos
ggplot(data_melt, aes(x = Date, y = Valor, color = Indicador)) +
  geom_line() +
  labs(title = "Indicadores Económicos Simulados", x = "Fecha", y = "Valor") +
  theme_minimal()

# 2. PIB Simulado vs Predicho
df_pib <- data.frame(
  Date = data$Date,
  Observado = data$PIB,
  Predicho = pib_pred_original_all
)
ggplot(df_pib, aes(x = Date)) +
  geom_line(aes(y = Observado, color = "Observado")) +
  geom_line(aes(y = Predicho, color = "Predicho"), linetype = "dashed") +
  labs(title = "PIB Simulado vs Predicho", x = "Fecha", y = "PIB") +
  scale_color_manual(values = c("blue", "red"), name = "Leyenda") +
  theme_minimal()

# 3. Evolución de Factores Latentes
factors_est_long <- melt(factors_est, id.vars = "Date", variable.name = "Factor", value.name = "Valor")
ggplot(factors_est_long, aes(x = Date, y = Valor, color = Factor)) +
  geom_line() + 
  labs(title = "Evolución de los Factores Latentes Estimados", x = "Fecha", y = "Valor") +
  theme_minimal()

# Predicción final
cat("Predicción del PIB para el próximo periodo (escala original):", pib_pred_original, "\n")
