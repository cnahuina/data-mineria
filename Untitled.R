# E2 MINERIA DE DATOS

## VER RMARKDOWN PUBLICADO https://rpubs.com/cnahuina/624753


diabetes=read.csv("https://raw.githubusercontent.com/VictorGuevaraP/Mineria-de-datos-2020/master/diabetes_m.csv", sep = ";", header = TRUE , fileEncoding = "Latin1", check.names = F)
names (telefonia)[2] = "Ocupacion"
head(diabetes)
summary(diabetes)

View(diabetes)

#Paso 1: Entendimiento del negocio (organización)
#1.1	Determinar el objetivo del negocio (organización)

# El objetivo de la minería de datos a aplicar para este proyecto 
# está en captar clientes que cuentan o contaron con el plan de 
# servicio(post pago) de la empresa Movistar, con el fin de predecir
# de una forma diagnóstica si un cliente es propenso a desafiliarse de cuya empresa.
# Esto basándose en mediciones ya adquiridas en el conjunto de los datos.

#1.2	Evaluar la situación actual (entorno)

# Se tiene un link del repositorio de gitHub en el cual se encuentra la base de datos
# con información de los clientes con un plan de servicio (post pago) por lo que despues se
# puede afirmar que se dispone de una cantidad de datos suficientes para poder resolver
# el problema. Esta información incluye el género del cliente, su ocupación, los minutos consumidos
# en el día llamadas, si cuenta con un plan internacion, la cantidad de minutos en llamadas internacionales,
# total llamadas internacionales realizadas,
# reclamos y si se encuentran desafiliados o no

#1.3	Determinar el objetivo de la minería de datos 

# Los objetivos en términos de minería de datos son:
# Predecir diagnósticamente si un cliente es propenso a desafiliarse de la empresa
# Identificar los clientes afiliados y no afiliados para un determinado estudio
# Comprobar con técnicas de predicción operaciones relacionadas con los clientes del plan de servicio
# Saber cuales son los índices que hacen que un cliente con plan Internacional decida realizar un desafilio
# del plan (migrar de un operador a otro).

#1.4	Realizar un plan de proyecto

# El proyecto estará dividido mediante las siguientes etapas, con la finalidad de facilitar la organización de este mismo:
# Etapa 1: Análisis de la estructura de los datos y la información de la base de datos. 
# Etapa 2: Ejecución de consultas para tener muestras representativas de los datos.
# Etapa 3: Preparación de los datos (selección, limpieza, conversión y formateo, si fuera necesario). 
# Etapa 4: Elección de las técnicas de modelado y ejecución de las mismas sobre los datos. 
# Etapa 5: Analisis de los resultados obtenidos en la etapa anterior, si fuera necesario repetir la etapa 4. 
# Etapa 6: Producción de informes con los resultados obtenidos en función de los objetivos de negocio y los criterios de éxito establecidos. 
# Etapa 7: Presentación de los resultados finales.

#Paso 2: Entendimiento de la data
#2.1	Información inicial de la data

str(diabetes)

# Identificación de los datos adquiridos:
# Género : Cada género está identificada sea en Masculino o femenino expresado textualmente.
# Ocupación: La ocupación está identificada de manera textual. 
# Plan Internacional: El plan internacional cuenta con sí y no(textualmente), cuenta con dos opciones en condición.
# Min_En_Dia: Los minutos al día están identificados por números decimales.
# Min_Internacionales: Los minutos internacionales están identificados también por números decimales.
# Reclamos: Los recla
# mos están expresados en números enteros.
# Llamadas_Internacionales: Los llamadas internacionales también están expresadas en números enteros

View(diabetes)

#2.2	Descripción de la data

summary(diabetes)

# El comando summary en el conjunto de datos nos indica que :
# Se pueden apreciar en el conjunto de datos proporcionados por la empresa Bitel que entre las personas que cuentan o contaron con un plan de servicio hay 1714 de género femenino y 1619 del masculino.
# En cuanto a la ocupación  se estima entre las personas del conjunto de datos con un total de 332 personas sin alguna ocupación específica(no asignada), 745 ocupados en la educación, 732 en los negocios, 727 en proyectos personales y 797 en otros tipos de ocupación.
# Entre las personas que cuentan con un plan de llamadas internacionales se estima un total de 323 clientes, por otra parte 3010 clientes no cuentan con este plan.
# La variable objetivo es “Desafiliado” ya que en base a esta columna se determinan por medio de “sí” y “no” el conjunto de personas que se encuentran y ya no afiliadas al plan de servicio de Bitel.
# Entre la cantidad de llamadas internacionales realizadas por los clientes se ha determinado por parte de uno un máximo de 20 llamadas, siendo aquella la más alta entre todas. 
# Los reclamos realizados por los clientes para este plan de servicio se contabilizaron determinando un promedio de 1.563, con una margen de 0 a 9 reclamos máximos realizados por cada cliente.
# La cantidad de clientes que están afiliados a este plan de servicio de Movistar son 2850, mientras que los ya no afiliados a este plan rondan de 483 clientes.


#2.3	Exploración de la data
install.packages("ggplot2")
library(ggplot2)

par(mfrow = c(1,1))

#Se muestran los valores atipicos de la data, se visualiza los valores atipicos (outliers) que se encuentra
#muy lejanos 
boxplot(diabetes)

#Con la funcion $stats describio los valores de los limites del boxplot (limInferior del bigote, el limInferior de la caja,
# la mediana del conjunto de valores , el limSuperior de la caja y el limSuperior del bigote), la $n el total de valores ,
# con $conf muestra los intervalos de confianza y $out la lista de outliers donde podemos visualizar el outlier mas bajo y alto

boxplot.stats(diabetes$NumeroEmbarazos)
boxplot.stats(diabetes$Presión.arterial.diastólica)
boxplot.stats(diabetes$edad)



#Se muestran los valores atipicos de la variable Min_en_dia
boxplot(telefonia$Min_En_Dia,horizontal = TRUE)
boxplot.stats(telefonia$Min_En_Dia)


#2.4	Verificación de la calidad de la data

library(VIM)
#En el cuadro de proporcion de missings podemos observar que la variable min_dia pasa del 0.06
#En el cuadro de proporcion de missings tambien podemos observar que la variable se mantiene menos del 0.05

#En el cuadro Combinations observamos los patrons de comportamiento en las variables min_en_dia donde varia en dos espacios
#En el cuadro Combinations observamos los patrons de comportamiento en las variables min_internacionales donde varia en dos espacios
missing=aggr(diabetes)

#resumen de la variable missing
summary(diabetes)


#Paso 3: Preparación de la data
#3.1	Seleccionar la data

newdiabetes = diabetes[,2:9]
#Se omitio la variable genero, ya que no es consecuente en la toma de decisiones, para una posible desafiliacion

#3.2	Limpiar la data

# Se crea una funcion para imputar outliers de el valor promedio del 95% del dataset, remplazandolos por la media y la mediana 
# sin tocar los NA, 
impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}
# Ejecutamos la funcion y asignamos para cada variable
newdiabetes$Min_En_Dia <- impute_outliers(newdiabetes$Min_En_Dia)
newdiabetes$Min_Internacionales <- impute_outliers(newdiabetes$Min_Internacionales)
newdiabetes$Reclamos <- impute_outliers(newdiabetes$Reclamos)
newdiabetes$Llamadas_Internacionales <- impute_outliers(newdiabetes$Llamadas_Internacionales)

par(mfrow = c(1,2))


#Comprobacion
boxplot(telefonia$Min_En_Dia, main = "Min_En_Dia con outliers")
boxplot(newtelefonia$Min_En_Dia, main = "Min_En_Dia sin outliers")

boxplot(telefonia$Min_Internacionales, main = "Min_Internacionales con outliers")
boxplot(newtelefonia$Min_Internacionales, main = "Min_Internacionales sin outliers")

boxplot(telefonia$Reclamos, main = "Reclamos con outliers")
boxplot(newtelefonia$Reclamos, main = "Reclamos sin outliers")$out

boxplot(telefonia$Llamadas_Internacionales, main = "Llamadas_Internacionales con outliers")
boxplot(newtelefonia$Llamadas_Internacionales, main = "Llamadas_Internacionales sin outliers")$out


# intentando quitar el outlier que queda
min_dia_sinoutliers = telefonia$Min_En_Dia[telefonia$Min_En_Dia < 326.3]
min_dia_sinoutliers2 = min_dia_sinoutliers[min_dia_sinoutliers > 36.0]
boxplot.stats(min_dia_sinoutliers2)
boxplot(min_dia_sinoutliers2)



#volver a una fila y una columna
par(mfrow = c(1,1))


install.packages("DMwR")
library(DMwR)

summary(newtelefonia)
telefonianew=knnImputation(newtelefonia)
summary(telefonianew)

#3.3	Construir data
install.packages("caret")
library(caret)

set.seed(123)
particion=createDataPartition(telefonianew$Desafiliado, p=0.70, times = 1, list = F)

dim(particion)

train=telefonianew[particion,]
test=telefonianew[-particion,]
dim(train)
dim(test)
#3.4	Integración de data

#3.5	Formato de data
write.csv(telefonianew, file = "dataPorFin.csv")
getwd()


#Paso 4: Modelamiento.

#Modelos de arbol general
install.packages("rpart")
library(rpart)

set.seed(123)

#evaluo la variable objetivo en funcion a todas las variables en base a la data de 
#entrenamiento, usando el metodo de tipo class

modelo1<-rpart(Desafiliado~.,data = train,method = "class")
modelo1
summary(modelo1)

install.packages("partykit")

library(partykit)
plot(as.party(modelo1))

head(test)
test


#Paso 5: Evaluación.

#predict: hace el recorrido de todos los modelos que le doy y predice
predicciones=predict(modelo1,test, type = "class")
predicciones
#Tabla de prediccioness
table(predicciones)


#comparacion junto de la test las predicciones
comparacion=cbind(test$Desafiliado, predicciones)

#Matriz de confusion
table(predicciones, test$Desafiliado)

accuracy=(839+50)/999
accuracy

#para saber en cuanto mi proyecto va a tener error
error_rate=1-accuracy
error_rate


##################################
#curva roc
str(test)
pred_prob<-predict(modelo1, test, type = "prob")[,2]
pred_prob
library(ROCR)
predR1 <- prediction(pred_prob, test$Desafiliado)
predR2<-performance(predR1, "tpr", "fpr")
plot(predR2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)

#Indice GINI
ROCRN <- round(performance(predR1, measure = "auc")@y.values[[1]]*100, 2)
giniRN <- (2*ROCRN - 100)
giniRN




