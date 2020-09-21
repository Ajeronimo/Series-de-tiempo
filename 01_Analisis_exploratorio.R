# Big Data  y Uso de Datos con R

# 1. Análisis exploratorio de datos
# 1.1 Obtención de datos
# 1.2 Analisis y limpieza de datos
# 1.3 datos faltantes
# 1.4 Diagrama de cajas y bigotes

#------------------------
#1.1 Obtención de datos

# lectura de archivos CSV (valores separados por comas)
# Utilizando R estandar sin usar ninguna libreria

rm(list = ls())

# Limpiamos la consola
cat("\014")

# Cambiamos el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#--------------
# https://finance.yahoo.com/
# Lectura de archivos (CSV)
inf_m <- read.csv("C:/Users/LENOVO/Documents/APA/SeisGradosData/INFY-monthly.csv")
#inf_m <- read.csv("INFY-monthly.csv")
str(inf_m)

#--------------
# Una forma alterna de obtener datos financieros
#install.packages("quantmod")
library(quantmod) # <- esta es la bibloteca que vamos a utilizar


mdate1="2001-01-04"
amazonprices1=getSymbols('AMZ', from=mdate1, auto.assign = F)

amazonprices1=getSymbols('AMZ', from=mdate1, auto.assign = F)[,4] # <- para obtener la columna 4  
print(amazonprices1)


#Bolsa Mexicana de Valores
getSymbols("^MXX", src = "yahoo", from="2010-01-01",to = "2020-09-07", periodicity="daily")
print(MXX)

chartSeries(MXX, TA=NULL)

chartSeries(MXX)
chartSeries(MXX,subset = "last 3 months")

#---------------
  # 1.2 Analisis y limpieza de datos
  # Identificacion de valores NA
  # 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
data <- read.csv("missing-data.csv",na.strings = "")
str(data)
mean(data$Income)  # No se puede aplicar porque tenemos valores NA

  # na.omit elimina las filas con valores NA
  # iternamente maneja a la fn. is.na devuelve un boolean
data.cleaned <-  na.omit(data)
mean(data.cleaned$Income)

is.na(data[4,2])   # devuelve True porque esa fila tiene un NA
is.na(data[4,1])   # devuelve False
is.na(data$Income) # Para columna Ingreso devuele los booleanos ingreso
data.income.cleaned <- data[!is.na(data$Income),]
mean(data.income.cleaned$Income)


  # 1.3 datos faltantes
  # Una solucion para los valores faltantes
  # Vamos a reemplazar los valores NA por el promedio
  # Creamos una columna que llena con el promedio 
  # cuando los valores son NA


data$Income.mean <- ifelse(is.na(data$Income),
                           mean(data$Income, na.rm = TRUE),
                           data$Income
)
View(data)

#-- 
# otra forma de resolver el problema de los datos faltantes es mediante
# el metodo locf(): last observatio carried forward

library(zoo)
data$Income.forward <- na.locf(data$Income)
view(data)

# O bien podemos usar
data$Income.NAfill <- na.fill(data$Income,55000)
view(data)


  # 1.4 Análisis de datos: Estadisticos, Gráficas y Diagramas de cajas y bigotes

inf_m <- read.csv("C:/Users/LENOVO/Documents/APA/SeisGradosData/INFY-monthly.csv")

summary(inf_m$Adj.Close)

# Otra forma
library(modeest) #moda
library(raster)  #quantiles, cv
library(moments) #asimetría, curtosis

x <-(inf_m$Adj.Close)
x

mean(x) # calculamos la media de sum(X)/length(X)


median(x) # una vez ordenados los datos es el valor que esta a la mitad
# si el número de datos es par se suman y se dividen entre dos

# moda: most frecuency values
mfv(x)

# la extensión de la mediana, es el percentil entre cero y uno
quantile(x)

# Medidas de dispersión varianza, desviación estándar,coeficiente de variación

var(x)
sd(x)
cv(x) # sd(X) / mean(X)

# Valores extremos (outliers)
library(forecast)
datos.outl <- tsoutliers(data$Income)
datos.outl  # en este caso no identifico outliers, habra que verificarlo con otro metodo.



# Medidas de simetría de una función
skewness(x) #  0.1037776 es ligeramente positiva

kurtosis(x) # -0.9873375 ligerament positiva-> leptocurtica
# puede ser también moments::kurtosis(X)

#Para darnos idea de como se ve la fn.
par(mfrow=c(1,1))
hist(x)
hist(x,prob= T)
lines(density(x))

hist(x,
     col = "blue",
     xlab = "Precio al Cierre Ajustado",
     ylab = "Frecuencias",
     main = "Histograma de Precios para XXX",
     breaks = 16) # Nos da el número de barras


# Boxplots
boxplot(x,
        xlab= "Precio Ajustado")

#---
# Otra forma de realizar el analisis de datos
install.packages("PASWR")

PASWR::EDA(x)
