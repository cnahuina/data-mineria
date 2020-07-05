################################################
#Modelamiento Predictivo
banco<-read.csv("https://raw.githubusercontent.com/VictorGuevaraP/ME-Machine-Learning/master/banco.csv", sep = ",")
head(banco)
# K-Vecinos más cercanos
bancoknn<-knnImputation(banco, k=10)
summary(bancoknn)
################################################

#creando train y test
###########################################################
#Verificamos la cantidad de datos
dim(bancoknn)
#Serealizara la división de 70% frente 30%
set.seed(123)
# Se crean los índices de las observaciones de entrenamiento
library(caret)
particion <- createDataPartition(y = bancoknn$suscrito, p = 0.7, list = FALSE, times = 1)
train <- bancoknn[particion, ]
test  <- bancoknn[-particion, ]
#Otra forma
#muestra<-sample(854,365)
#Data de entrenamiento
#train1<-bancoknn[-muestra,]
#Data de prueba
#test1<-bancoknn[muestra]
#Verificamos las dimensiones
dim(train)
dim(test)



############################################################
##########################################################
# M O D E L O S ##########################################

#MODELO DE REGRESIÓN LOGISTICA
# Estimacion
names(bancoknn)
modeloRL <- glm(suscrito~., family=binomial,data=bancoknn)
summary(modeloRL)

#Interpretación:
# - Para cada cambio unitario en duración, el logaritmo de la ventaja de suscribirse
#   (versus no Desafiliarse) se incrementa en 0.0298.
# - Para un cambio unitario en la Edad, el logaritmo de la ventaja de Suscribirse
#   de la empresa de telecomunicaciones disminuye en  0.0133


# Intervalos de Confianza para los coeficientes
confint(modeloRL)
## solo tasa de ventajas
exp(coef(modeloRL))
## tasa de ventajas e IC 95% 
exp(cbind(OR = coef(modeloRL), confint(modeloRL)))

#Interpretación:
# De los resultados es posible afirmar que ante un cambio unitario en la duracion,
# la ventaja de suscribirse a la empresa (frente a no suscribirse)
# se incrementa en un factor de 1.030261  (3.02%)


# Selección de Variables
require(MASS)
stepAIC(modeloRL)

# Calidad de Ajuste (Prueba de Hosmer y Lemeshow)
library(ResourceSelection)
hl <- hoslem.test(modeloRL$y, fitted(modeloRL), g=18)
hl
cbind(hl$observed,hl$expected)

#RCreamos un modelo pero con la data train
modelo.trainRL<-glm(suscrito~.,family = binomial, data = train)
summary(modelo.trainRL)

#Probabilidades predichas en la nueva data
predichosRL<- predict(modelo.trainRL, test, type = "response")
predichosRL

## Matriz de confusión (error Clasificación)
# Valores predichos de Y
ypred <- as.numeric((predict(modelo.trainRL, newdata=test[,-19], type="response") >= 0.5) )
# Otra forma:
#ypred <- as.numeric(predict(modelo.trainRL, bancoknn[,-19])>0)

ytrue <- test$suscrito
testerr <- mean(ypred!=ytrue)
testerr

#Base de datos con las probabilidades y categorias predichas
yprob <-  predict(modelo.trainRL, newdata = test[,-19], type = "response")

banco.new<- as.data.frame(cbind(test,ypred,yprob))
table(test$suscrito, ypred)

#Indicadores
library(caret)
predL2<-ifelse(predichosRL<=5,0,1)
predL2<-as.factor(predL2)
resulA <- confusionMatrix(predL2, test$suscrito)
resulA

#Para la curva roc
predRL<-predict(modelo.trainRL, test, type = "response")
library(ROCR)
predRL1 <- prediction(predRL, test$suscrito)
predRL2<-performance(predRL1, "tpr", "fpr")

plot(predRL2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)

## Hallar punto de corte (cutoff) optimo. Solo funciona para la reg. logistica
library(pROC)
library(Epi)
ROC(form=suscrito~., data=test)
?ROC
#Indice gini
ROCRL <- round(performance(predRL1, measure = "auc")@y.values[[1]]*100, 2)
giniRL <- (2*ROCRL - 100)
giniRL

##################
## Otros enlaces##
##################
library(bestglm)
modelo2RL<- glm(suscrito~., family=binomial(link=probit),data=bancoknn)
summary(modelo2RL)
ypred <- as.numeric(predict(modelo2RL, bancoknn[,-19])>0)
ytrue <- bancoknn$suscrito
testerr <- mean(ypred!=ytrue)
testerr

modelo3RL <- glm(suscrito~., family=binomial(link=cloglog),data=bancoknn)
summary(modelo3RL)
ypred <- as.numeric(predict(modelo3RL, bancoknn[,-19])>0)
ytrue <- bancoknn$suscrito
testerr <- mean(ypred!=ytrue)
testerr


############################################################
#mODELOS DE ARBOLES DE DESICIÓN
names(bancoknn)
#Creamos modelo de árboles
library(rpart)
modeloA<-rpart(suscrito~.,data =train,method = "class")
#Predecimos utilizando el modelo
predA<-predict(modeloA, test, type = "class")

# Matriz de confusión
tablaA<-table(predA, test$suscrito)
tablaA

#Indicadores
library(caret)
resulA <- confusionMatrix(predA, test$suscrito)
resulA

#Para la curva roc
predAA<-predict(modeloA, test, type = "prob")[,2]
library(ROCR)
predA1 <- prediction(predAA, test$suscrito)
predA2<-performance(predA1, "tpr", "fpr")

plot(predA2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)


AUROC <- round(performance(predA1, measure = "auc")@y.values[[1]]*100, 2)
giniA <- (2*AUROC - 100)
giniA

library(ROSE)
roc.curve(test$suscrito, predAA, plotit = F)


##################################################################
#################################################################
#Creamos modelo de NAIVE BAYES
library(e1071)
modeloB<-naiveBayes(suscrito~.,data =train,method = "class")
#Predecimos utilizando el modelo
predB<-predict(modeloB, test, type = "class")

##########################################
#  Matriz de confusión
tablaB<-table(predB, test$suscrito)
tablaB
############################################
#Indicadores
library(caret)
resulB <- confusionMatrix(predB, test$suscrito)
resulB

##################################
#Para la curva roc
predBB<-predict(modeloB, test, type = "raw")[,2]
library(ROCR)
predB1 <- prediction(predBB, test$suscrito)
predB2<-performance(predB1, "tpr", "fpr")

plot(predB2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)


ROCB <- round(performance(predB1, measure = "auc")@y.values[[1]]*100, 2)
giniB <- (2*ROCB - 100)
giniB

###########################################################################
##########################################################################
#Creamos modelo de ANR
library(nnet)
modeloR<-nnet(suscrito~ ., train, size=10 ,trace=FALSE, maxit=1000)
#Predecimos utilizando el modelo
predR<-predict(modeloR, test, type = "class")
##########################################
#antes####
tablaR<-table(predR, test$suscrito)
tablaR
############################################
#Indicadores
library(caret)
predR<-as.factor(predR)
resulR <- confusionMatrix(predR, test$suscrito)
resulR

##################################
#Para la curva roc
predRN<-predict(modeloR, test, type = "raw")
library(ROCR)
predR1 <- prediction(predRN, test$suscrito)
predR2<-performance(predR1, "tpr", "fpr")
plot(predR2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)


ROCRN <- round(performance(predR2, measure = "auc")@y.values[[1]]*100, 2)
giniRN <- (2*ROCRN - 100)
giniRN

par(mfrow=c(2,2))
plot(predA2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)
plot(predB2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)
plot(predR2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)


#Para la curva roc
predRF<-predict(modeloRF, test, type = "prob")[,2]
library(ROCR)

predRF1 <- prediction(pred, test$suscrito)
predRF2<-performance(predRF1, "tpr", "fpr")

plot(predRF2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)


ROCRF <- round(performance(predR, measure = "auc")@y.values[[1]]*100, 2)
giniRF <- (2*ROCRF - 100)
giniRF
