# PARCIAL MINERIA DE DATOS

## TOMADO DESDE DATOSABIERTOS.GOB.PE
## https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa

## El conjunto de datos publicado corresponde Fecha de Nacimiento, Fecha de Fallecimiento, Sexo, Departamento, Provincia y Distrito. 
## Fuente: Centro Nacional de Epidemiologia, prevención y Control de Enfermedades – MINSA.
## Frecuencia de actualización 
## La información se actualiza diariamente
## fecha: 12/6/2020.  


fcovid=read.csv("https://raw.githubusercontent.com/cnahuina/data-mineria/master/fallecidos_covid.csv", sep = ",", header = TRUE , fileEncoding = "Latin1", check.names = F)
head(fcovid)
summary(fcovid)

View(fcovid)

#Paso 1: Entendimiento de la data (fallecidos por covid)
#1.1	Determinar el objetivo de la organizacion (MINSA)

#1.2	Evaluar la situación actual (entorno)

#1.3	Determinar el objetivo de la minería de datos 

#1.4	Realizar un plan de proyecto

#Paso 2: Entendimiento de la data
#2.1	Información inicial de la data

str(fcovid)

#2.2	Descripción de la data

summary(fcovid)

#2.3	Exploración de la data
install.packages("ggplot2")
library(ggplot2)

par(mfrow = c(1,1))

#Se muestran los valores atipicos de la data, se visualiza los valores atipicos (outliers) que se encuentra
#muy lejanos 
boxplot(fcovid)

boxplot.stats(fcovid$EDAD_DECLARADA)
boxplot.stats(fcovid$SEXO)
boxplot.stats(fcovid$DEPARTAMENTO)

#2.4	Verificación de la calidad de la data

library(VIM)
missing=aggr(fcovid)

#resumen de la variable missing
summary(fcovid)

#Paso 3: Preparación de la data
#3.1	Seleccionar la data

newfcovid1 = fcovid[,(2:4)]
#Se omitio la variable genero, ya que no es consecuente en la toma de decisiones, para una posible desafiliacion

#3.2	Limpiar la data


