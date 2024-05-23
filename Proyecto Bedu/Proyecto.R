#Ir al directorio
setwd("C:/Users/Javier/#BEDU/Modulo 4/")

#Librerías a utilzar
library(ggplot2)
library(lubridate)
library(dplyr)


# Cargar el archivo CSV certificaciones 
qa.certs <- read.csv("certs_clean.csv")

# Convertir a dataframe y almacenar en variables
qadf.certs <- as.data.frame(qa.certs)

# Análisis de los dataframes creados
names(qadf.certs)

#Conversión a fecha de la columna Created Date
qadf.certs$Created.Date <- as.Date(qadf.certs$Created.Date, "%d/%m/%Y")  

#----------PLANTEAMIENTO DEL PROBLEMA-------------------
#
#Actualmente dentro del área de QA se realizan estimados de los tiempos que requerirá cada certificación/ejecución de pruebas 
# para cada requerimiento por separado, y en algunas ocasiones esta estimación puede quedarse corta o por el contrario 
# tener demasiada holgura, en ambos casos este desfase puede significar una planificación erronea del resto del tiempo disponible 
# para las demás actividades. Es por esto que un modelo que pueda predecir con mayor certeza el tiempo que tomará 
# realizar dichas pruebas podría ayudar significativamente a las planificaciones que se realizan previo a la 
# ejecución de pruebas o documentación de los incidentes o atención de requerimientos.

#----------ANÁLISIS DEL DATAFRAME------------------------

# Histograma para el campo Esfuerzo_certificate
hist(qadf.certs$Esfuerzo_certificate, main = "Distribución del Esfuerzo", xlab = "Esfuerzo")

# Gráfico de barras para el campo Tipo de Incidente
barplot(table(qadf.certs$Tipo.de.Incidente), main = "Tipos de Incidente", xlab = "Tipo de Incidente")

# Filtrar el dataframe para incluir solo los valores de "Evolutivo" y "Express"
df_evexp <- qadf.certs[qadf.certs$Tipo.de.Incidente %in% c("Evolutivo", "Express"), ]

# Gráfico de barras para el campo Esfuerzo, filtrado por los Tipos de Incidente
ggplot(df_evexp, aes(x = Tipo.de.Incidente, y = Esfuerzo_certificate, fill = Tipo.de.Incidente)) +
  geom_bar(stat = "identity") +
  labs(title = "Esfuerzo por Tipo de Incidente (Evolutivo y Express)",
       x = "Tipo de Incidente", y = "Esfuerzo") +
  theme_minimal()


# Promedio de Esfuerzo para certificaciones 
mean(df_evexp$Esfuerzo_certificate)

# Boxplot para Esfuerzo_certificate por Tipo de Incidente
ggplot(df_evexp, aes(x = Tipo.de.Incidente, y = Esfuerzo_certificate)) +
  geom_boxplot() +
  labs(title = "Boxplot: Esfuerzo por Tipo de Incidente")


# Calcular mu y sigma
mu <- mean(df_evexp$Esfuerzo_certificate)
sigma <- sd(df_evexp$Esfuerzo_certificate)

#DISTRIBUCIÓN NORMAL
par(mfrow = c(2, 2))

# Crear histograma
hist(df_evexp$Esfuerzo_certificate, breaks = 20, prob = TRUE,
     main = "Distribución de Esfuerzo",
     xlab = "Esfuerzo", ylab = "Densidad")
title( sub = expression(paste(mu == 13.4964, " y ", sigma == 38.77)))
curve(dnorm(x, mean = mudf, sd = sigmadf), col = "blue", lwd = 2, add = TRUE)


#PROBABILIDAD DE X<=50
# Crear polígono para la probabilidad de Esfuerzo <= 50
x <- seq(0, 50, length.out = 100)
hx <- dnorm(x, mean = mu, sd = sigma)
polygon(c(0, x, 50), c(0, hx, 0), col = "red", border = NA)

# Agregar etiqueta de probabilidad
area <- pnorm(50, mean = mu, sd = sigma)
result <- paste("P(Esfuerzo <= 50) =", signif(area, digits = 3))
mtext(result, 3)


#PROBABILIDAD DE 25 <= X <= 75
hist(df_evexp$Esfuerzo_certificate, breaks = 20, prob = TRUE,
     main = "Distribución de Esfuerzo",
     xlab = "Esfuerzo", ylab = "Densidad")
title( sub = expression(paste(mu == 13.4964, " y ", sigma == 38.77)))
curve(dnorm(x, mean = mudf, sd = sigmadf), col = "blue", lwd = 2, add = TRUE)

# Crear polígono para la probabilidad de Esfuerzo 25 <= X <= 75
x <- seq(25, 75, length.out = 100)
hx <- dnorm(x, mean = mu, sd = sigma)
polygon(c(25, x, 75), c(0, hx, 0), col = "yellow", border = NA)

# Agregar etiqueta de probabilidad
area <- pnorm(25, mean = mu, sd = sigma) - pnorm(75, mean = mu, sd = sigma)
result <- paste("P(25 <= X <= 75) =", signif(area, digits = 3))
mtext(result, 3)


#PROBABILIDAD DE X >= 100
hist(df_evexp$Esfuerzo_certificate, breaks = 20, prob = TRUE,
     main = "Distribución de Esfuerzo",
     xlab = "Esfuerzo", ylab = "Densidad")
title( sub = expression(paste(mu == 13.4964, " y ", sigma == 38.77)))
curve(dnorm(x, mean = mudf, sd = sigmadf), col = "blue", lwd = 2, add = TRUE)

# Crear polígono para la probabilidad de Esfuerzo >= 100
x <- seq(100, max(df_evexp$Esfuerzo_certificate), length.out = 100)
hx <- dnorm(x, mean = mu, sd = sigma)
polygon(c(100, x[x>=100], max(x)), c(0, hx, 0), col = "green", border = NA)

# Agregar etiqueta de probabilidad
area <- pnorm(100, mean = mu, sd = sigma, lower.tail = FALSE)
result <- paste("P(Esfuerzo >=100) =", signif(area, digits = 3))
mtext(result, 3)

#--------CONTRASTE DE HIPOTESIS------------------------
#El Esfuerzo tiene una media de 50
# Obtener Muestra
set.seed(29)
x_h0 <- rnorm(df_evexp$Esfuerzo_certificate, mean = 50)

# Realizar el t-test de una muestra

t_test_result <- t.test(x = x, mu = 50, alternative = "two.sided", conf.level = 0.95)

# Imprimir los resultados
print(t_test_result)


#--------------------------------------------
#La media del esfuerzo entre Evolutivo y Express es igual 
df_ev <- qadf.certs[qadf.certs$Tipo.de.Incidente %in% c("Evolutivo"), ]
df_exp <- qadf.certs[qadf.certs$Tipo.de.Incidente %in% c("Express"), ]

#Generación de muestras
express <- rnorm(length(df_exp$Esfuerzo_certificate), mean = mean(df_exp$Esfuerzo_certificate))
evolutivo <- rnorm(length(df_ev$Esfuerzo_certificate), mean = mean(df_ev$Esfuerzo_certificate))

# Realizar el t-test para muestras independientes
t_test_result <- t.test(x = express, y = evolutivo, alternative = "two.sided", conf.level = 0.95)

# Imprimir los resultados
print(t_test_result)


#------------IMPLEMENTACIÓN REGRESIÓN LINEAL MULTIPLE----------------
attach(qadf.certs)
modelo <- lm(Esfuerzo_certificate ~ useCaseTesting_certificate + uatTest_certificate + Tipo.de.Incidente + serviceTesting_certificate)
summary(modelo)

#---Ajuste de modelo
modelo2 <- lm(Esfuerzo_certificate ~ useCaseTesting_certificate + uatTest_certificate + Tipo.de.Incidente)
summary(modelo2)


#---Análisis de Covarianza
modelo3 <- lm(Esfuerzo_certificate ~ useCaseTesting_certificate + uatTest_certificate + Tipo.de.Incidente +
                uatTest_certificate:useCaseTesting_certificate + Tipo.de.Incidente:useCaseTesting_certificate)
summary(modelo3)

