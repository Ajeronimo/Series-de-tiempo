rm(list = ls())

# Limpiamos la consola
cat("\014")

# Cambiamos el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Librerias basicas para el estudio de series temporales
# ------------------------------------------------------
# ------------------------------------------------------
library(TSstudio)
library(forecast)
library(lmtest)
library(rlang)


# descripcion de la data
data=read.csv("petroleo.csv",header = T,sep=";",dec=".")
head(data,5)
tail(data,5)

data.ts <- ts(data = data$petroleo, start = c(2010,1),end = c(2019,12), frequency = 12 )
data.ts
plot(data.ts)


# Particion la serie training/testing
# -----------------------------------
data.ts.part = ts_split(data.ts, sample.out = 12)
train.ts = data.ts.part$train
test.ts = data.ts.part$test



# Optimizadores AUTO ARIMA
# -------------------------
# -------------------------
model1 <- auto.arima(train.ts)
print(model1)

# Series: train.ts 
# ARIMA(1,1,0)(0,0,1)[12]

# Graficamos
plot.ts(data.ts, main = 'Precio petroleo', col = 'blue')
lines(fitted(model1),col = 'red')

# Forecast
# --------
forecast_arima <- forecast(model1, h = 12)
forecast_arima

# Rendimiento del modelo
# ----------------------
test_forecast(actual = data.ts,
              forecast.obj =  forecast_arima,
              test = test.ts)
# Podemos observar que el modelo no se ajusta adecuadamante para los ultimos periodos
 
# Al parecer el modelo esta influenciado por los retardos, pero no precisamente 
# de los datos que están cerca, sino de aquellos que estan varios rezagos a tras


# SARIMAX
# ------------------
# ------------------
# trabajando con regresores
library(data.table)

data_regre <- as.data.frame(train.ts)

data_regre$x1 <- shift(data_regre$x, n = 1, type = 'lag')
data_regre$x2 <- shift(data_regre$x, n = 2, type = 'lag')

# Vamos a imputa los valores faltantes
data_regre$x1[1] <- 96.66   
data_regre$x2[1] <- 96.66
data_regre$x2[2] <- 96.66
view(data_regre)

as.matrix(data_regre[c('x1','x2')]) # Seleccionamos variables como regresores


model02 <- auto.arima(train.ts, xreg = as.matrix(data_regre[c('x1','x2')]))
print(model02)

# Regression with ARIMA(0,0,0)(0,0,1)[12] errors

# Forecast
# --------
data_regre_test <- as.data.frame(test.ts)

data_regre_test$x1 <- shift(data_regre_test$x, n = 1, type = 'lag')

data_regre_test$x2 <- shift(data_regre_test$x, n = 2, type = 'lag')

tail(train.ts)  # para ver el ultimo dato de la data de train

data_regre_test$x1[1] <- 95.92   # Rellenamos con el ultimo dato
data_regre_test$x2[1] <- 95.92
data_regre_test$x2[2] <- 95.92

# Forecast
# --------

forecast_sarima <- forecast(model02, h = 12,
                            xreg = as.matrix(data_regre_test[c('x1','x2')]))

test_forecast(actual = data.ts,
              forecast.obj = forecast_sarima,
              test = test.ts)


# Neural network models (redes artificiales del tipo perceptron)
# Tenemos que escalarla para tener pesos
# ---------------------
# ---------------------


model02_nn <- nnetar(train.ts, scale.inputs = TRUE)

# model02_nn <- nnetar(train.ts, scale.inputs = TRUE, lambda = "auto")

print(model02_nn)

forecast_nn = forecast(model02_nn, h = 12)
plot_forecast(forecast_nn)

forecast_nn$fitted

checkresiduals(model02_nn)



