################################################################################
#
#                            PROYECTO FINAL. SERIES DE TIEMPO
#                                        CIMAT
#                                NELSON ARIZA MORALES 
#                             ESTIMACION DEL PIB TRIMESTRAL
#                                      FORECASTING
#
################################################################################

library(readxl)
library(ggplot2)
library(forecast)
library(tseries)


# Cargar el archivo y visualizar
PIB_data <- read_excel("C:\\Users\\nmari\\Documents\\CIMAT\\Computo E\\Examen_01\\PIB_Trimestral.xlsx")
head(PIB_data)

PIB_data$Fecha <- as.Date(paste0(PIB_data$Año, "-", 
                                 ifelse(PIB_data$Trimestre == "T1", "01", 
                                        ifelse(PIB_data$Trimestre == "T2", "04", 
                                               ifelse(PIB_data$Trimestre == "T3", "07", "10"))), "-01"))


ggplot(PIB_data, aes(x = Fecha, y = PIB)) +
  geom_line(color = "darkblue") +
  labs(title = "PIB Trimestral de México", x = "Fecha Trimestral", y = "PIB") +
  theme_minimal()


# PRUEBAS ACF Y PACF
pib_ts <- ts(PIB_data$PIB, start = c(1980, 1), frequency = 4)
acf(pib_ts, main = "ACF del PIB Trimestral de México")
pacf(pib_ts, main = "PACF del PIB Trimestral de México")


#PRUEBA DE ESTACIONALIDAD
  
# Aplicar la prueba ADF
adf_test <- adf.test(pib_ts, alternative = "stationary")
print(adf_test)
help("adf.test") # p = 0.04, la serie es estacionaria 

# Crear la matriz de regresión
nY <- length(pib_ts)
Tend <- 1:nY  # Tendencia
Trimestres <- factor(cycle(pib_ts))  # Estacionalidad trimestral

# Incorporación de variables dummy para crisis económicas
# 1994 (TLC), 2008 (crisis financiera mundial), y 2020 (COVID-19)
crisis_1994 <- ifelse(PIB_data$Año == 1994 & PIB_data$Trimestre %in% c("T2", "T3", "T4"), 1, 0)
crisis_1995 <- ifelse(PIB_data$Año == 1995 & PIB_data$Trimestre %in% c("T2", "T3", "T4"), 1, 0)
crisis_1987 <- ifelse(PIB_data$Año == 1987 & PIB_data$Trimestre %in% c("T2", "T3", "T4"), 1, 0)
crisis_1982 <- ifelse(PIB_data$Año == 1982 & PIB_data$Trimestre %in% c("T2", "T3", "T4"), 1, 0)
crisis_1983 <- ifelse(PIB_data$Año == 1983 & PIB_data$Trimestre %in% c("T2", "T3", "T4"), 1, 0)
crisis_2008 <- ifelse(PIB_data$Año == 2008 & PIB_data$Trimestre %in% c("T2", "T3", "T4"), 1, 0)
crisis_2009 <- ifelse(PIB_data$Año == 2009 & PIB_data$Trimestre %in% c("T2", "T3", "T4"), 1, 0)
crisis_2020 <- ifelse(PIB_data$Año == 2020 & PIB_data$Trimestre %in% c("T2", "T3", "T4"), 1, 0)

# Ajustar el modelo
X <- data.frame(Intercepto = rep(1, nY), Tend = Tend, Trimestres = Trimestres, 
                Crisis1994 = crisis_1994, Crisis1995 = crisis_1995,
                Crisis1982 = crisis_1982, Crisis1983 = crisis_1983,
                Crisis2008 = crisis_2008, Crisis2009 = crisis_2009, 
                Crisis1987 = crisis_1987, Crisis2020 = crisis_2020)


RR <- lm(pib_ts ~ Tend + Trimestres + Crisis1994 + Crisis1987 + Crisis1995 + Crisis1982 + Crisis1983 + Crisis2009 + Crisis2008 + Crisis2020, data = X)
summary(RR)
RES <- residuals(RR)


# Aplicar la prueba ADF
adf_test <- adf.test(RES, alternative = "stationary")
print(adf_test) 

valores_ajustados <- fitted(RR)

#  PIB vs valores ajustados
plot(PIB_data$Fecha, pib_ts, type = "l", col = "blue", lwd = 2,
     main = "PIB Trimestral de México: Datos Observados vs Modelo Ajustado",
     xlab = "Fecha", ylab = "PIB", ylim = range(c(pib_ts, valores_ajustados)))
lines(PIB_data$Fecha, valores_ajustados, col = "red", lwd = 2)


# Residuales
plot.ts(RES, main = "Residuales del Modelo con Tendencia, Estacionalidad y Crisis", ylab = "Residuales")
acf(RES, main = "ACF de los Residuales")
pacf(RES, main = "PACF de los Residuales")

# Modelo ARMA
amo <- auto.arima(RES)
arma_11 <- arima(RES, order = c(1, 0, 1))
summary(arma_11)

# ARMA
residuos_arma <- residuals(arma_11)
plot.ts(residuos_arma, main = "Residuos del Modelo ARMA(1,1)", ylab = "Residuos")

# PRONOSTICO
n_pronostico <- 12 # Próximos 12 trimestres (3 años)
pronostico_residuos <- predict(arma_11, n.ahead = n_pronostico)
fechas_pronostico <- seq(from = max(PIB_data$Fecha), by = "quarter", length.out = n_pronostico)

# Crear nuevas variables de Tendencia y Trimestres para los próximos 12 trimestres
Tend_pronostico <- (nY + 1):(nY + n_pronostico)
Trimestres_pronostico <- factor(rep(1:4, length.out = n_pronostico))

# Asumimos que las crisis no continúan en los próximos años
crisis1994_pronostico <- rep(0, n_pronostico)
crisis2008_pronostico <- rep(0, n_pronostico)
crisis2020_pronostico <- rep(0, n_pronostico)
crisis1995_pronostico <- rep(0, n_pronostico)
crisis1982_pronostico <- rep(0, n_pronostico)
crisis1983_pronostico <- rep(0, n_pronostico)
crisis2009_pronostico <- rep(0, n_pronostico)
crisis1987_pronostico <- rep(0, n_pronostico)

# Crear el data frame del pronóstico
X_pronostico <- data.frame(Intercepto = rep(1, n_pronostico), 
                           Tend = Tend_pronostico, 
                           Trimestres = Trimestres_pronostico,
                           Crisis1994 = crisis1994_pronostico, 
                           Crisis2008 = crisis2008_pronostico,
                           Crisis1995 = crisis1995_pronostico,
                           Crisis1982 = crisis1982_pronostico,
                           Crisis1983 = crisis1983_pronostico,
                           Crisis2009 = crisis2009_pronostico,
                           Crisis1987 = crisis1987_pronostico,
                           Crisis2020 = crisis2020_pronostico)

pronostico_deterministico <- predict(RR, newdata = X_pronostico)
pronostico_final <- pronostico_deterministico + pronostico_residuos$pred

#  Graficar
plot(PIB_data$Fecha, pib_ts, type = "l", col = "blue", lwd = 2,
     main = "Pronóstico del PIB Trimestral de México",
     xlab = "Fecha", ylab = "PIB", ylim = range(c(pib_ts, pronostico_final)))
lines(fechas_pronostico, pronostico_final, col = "red", lwd = 2, lty = 2)
lines(fechas_pronostico, pronostico_final + 2 * pronostico_residuos$se, col = "grey", lty = 2)
lines(fechas_pronostico, pronostico_final - 2 * pronostico_residuos$se, col = "grey", lty = 2)
legend("topleft", legend = c("Datos observados", "Pronóstico", "Intervalo de confianza"),
       col = c("blue", "red", "grey"), lty = c(1, 2, 2), lwd = c(2, 2, 2))



