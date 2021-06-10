#This script uses different methods to fit a classification tree to our data set
 dir.data<-"C:/Users/cinth/Documents/BCDD/Evidencia1/Data/Model/"
 dataO<-read.csv(paste0(dir.data,"ModelData.csv"))

#let's look at the data first,
  summary(dataO)

#remove columns with only NA
 bad.vars<-sapply(dataO, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
 bad.vars<-names(bad.vars[bad.vars>0.03])
 bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#let's select a response of interest
#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
  response<-"EN.ATM.CO2E.PC"
  #  predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))
  predictors<-subset(colnames(dataO), colnames(dataO)%in%c("BX.KLT.DINV.CD.WD", "SP.POP.TOT2L", "a_energy", "a_urban", "a_transport", "a_social_development", "a_education", "a_environment", "m_waste", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG", "SP.POP.GROW"))

# formar el conjunto de entrenamiento con países en vías de desarrollo 
data <- dataO[c(4, 150, 6, 8, 19, 25, 18, 24, 26, 32, 38, 5, 138, 78, 77, 88, 95, 124, 112, 108, 118, 127, 134, 139, 136, 148, 167, 192, 171, 176, 178, 183, 33),c(predictors,response)] 

# formar el conjunto de prueba con el resto de los países
nic<- c(4, 150, 6, 8, 19, 25, 18, 24, 26, 32, 38, 5, 138, 78, 77, 88, 95, 124, 112, 108, 118, 127, 134, 139, 136, 148, 167, 192, 171, 176, 178, 183, 33)
data.test <- dataO[!seq(1,nrow(dataO)) %in% nic,c(predictors,response)] 
data.test

dim(data) # el conjunto de prueba tiene 33 observaciones y 12 columnas
predictors
# View(data)

# Since we are analyzing this as a classification problem
#let's look at how this response is distributed
  summary(data[,response])
  data <- data[complete.cases(data),] # quitamos las observaciones con NAs
  dim(data) # nos quedan 31 observaciones 

  # PARA EL CONJUNTO DE ENTRENAMIENTO (31 PAISES) creamos una variable binaria que nos diga si un país tiene emisiones altas o bajas considerando un límite del 50% de los datos
  # summary(data[,response])
  # plot(density(data[,response]))
threshold<-as.numeric(quantile(data[,response],0.5,na.rm=TRUE))
data$response.binary<-as.factor(ifelse(data[,response]>threshold,"High","Low"))
summary(data$response.binary)
# quantile(data[,response],0.5,na.rm=TRUE)

  # se repite lo anterior PARA EL CONJUNTO DE PRUEBA (150 PAISES)
threshold.2 <-as.numeric(quantile(data.test[,response],0.5,na.rm=TRUE))
data.test$response.binary<-as.factor(ifelse(data.test[,response]>threshold,"High","Low"))
summary(data.test$response.binary)


# PARA EL CONJUNTO DE ENTRENAMIENTO
# remove NA values in the RESPONSE
 data<-subset(data,is.na(response.binary)==FALSE)
dim(data)
#now look at how many high emission cases we have
 summary(data$response.binary)

#Now before we start modeling, we subset the data to be used in the model to only complete cases
 data.model<-data[,c("response.binary",predictors)]
 data.model<-data.model[complete.cases(data.model),]
 summary(data.model)
 dim(data.model)
 
 # PARA EL CONJUNTO DE PRUEBA se repite lo anterior
 data.model.2 <- data.test[,c("response.binary",predictors)]
 data.model.2 <- data.model.2[complete.cases(data.model.2),]
 summary(data.model.2)
 dim(data.model.2)
 summary(data.model.2$response.binary)

 data.test<-subset(data.test,is.na(response.binary)==FALSE)
 dim(data.test)
 
 data.test <- data.test[complete.cases(data.test),]
 summary(data.test)
 dim(data.test)

# now we have a clean data set

#define the model we want to estimate
 model<-as.formula(paste0("response.binary","~",paste(predictors,collapse="+")))
 model
 
# Creamos el árbol
 library(tree)
 set.seed (55555)
 tree.model <- tree(model,data.model) 
 tree.model
 response.binary.model<-data.model$response.binary # vemos los valores que toman los 80 datos de prueba en la columna "response.binary"
 
 tree.model.pred<-predict(tree.model ,data.model,type ="class") 
 
 table(tree.model.pred ,response.binary.model)
 plot(tree.model)
 text(tree.model,pretty =0)
# So this tree has an error rate of (3+3)/31 = 0.194
 
 # Aplicamos Random forest
 library(randomForest)
 rf.data.model <- randomForest(model,
                               data=data.model,
                               mtry=round(length(predictors)^0.5),
                               importance =TRUE
 )
 rf.data.model
 #base on many different trees, which are the most important drivers
 importance (rf.data.model)
 varImpPlot (rf.data.model )
 
 ## "VALIDACIÓN" DEL MODELO: usamos las siguientes 2 formas
 
# 1. aquí usamos 31 de los 150 países restantes para probar nuestro modelo ---------

 response.binary.test<-data.model.2$response.binary
 response.binary.test
 tree.model.pred
 
 test <- sample(response.binary.test, 31, replace = FALSE)
 
# MATRIZ DE CONFUSIÓN PREDICCIÓN Y PRUEBA
  table(tree.model.pred, test)
# el MSE de prueba es (4+12)/31 = 0.516
  
# ---------------------------------------------------------------------------------
  
# 2. aquí hacemos un árbol usando solo los 150 países restantes para comparar con nuestro árbol original ---------
  
  threshold.3 <-as.numeric(quantile(data.test[,response],0.5,na.rm=TRUE))
  data.test$response.binary<-as.factor(ifelse(data.test[,response]>threshold,"High","Low"))
  summary(data.test$response.binary)
  
  data.test<-subset(data.test,is.na(response.binary)==FALSE)
  dim(data.test)
  #now look at how many high emission cases we have
  summary(data.test$response.binary)
  
  #Now before we start modeling, we subset the data to be used in the model to only complete cases
  data.model.3<-data.test[,c("response.binary",predictors)]
  data.model.3<-data.model.3[complete.cases(data.model.3),]
  summary(data.model.3)
  dim(data.model.3)
  
 summary(data.model.3$response.binary)

 set.seed (55555)
 tree.model.3 <- tree(model,data.model.3) # se usan el modelo, la base de datos y el subconjunto
 tree.model.pred3 <-predict(tree.model.3 ,data.model.3 ,type ="class") # probamos qué tan buen modelo es dándole el árbol estimado, el conjunto de prueba y type
 table(tree.model.pred3 ,data.model.3$response.binary)
 plot(tree.model.3)
 text(tree.model.3  ,pretty =0)
 
 # Random forest 
 library(randomForest)
 rf.data.model.3 <- randomForest(model,
                               data=data.model.3,
                               mtry=round(length(predictors)^0.5),
                               importance =TRUE
 )
 rf.data.model.3
 
 #base on many different trees, which are the most important drivers
 importance (rf.data.model.3)
 varImpPlot (rf.data.model.3 )
 
# Vemos si al utilizar cross-validation y cortar el árbol la predicción mejora
 
 set.seed (55555)
 cv.data.model<-cv.tree(tree.model.3 ,FUN=prune.misclass )
 cv.data.model
 
 prune.tree.model.3 <- prune.misclass (tree.model.3,best =11)
 plot(prune.tree.model.3 )
 text(prune.tree.model.3 ,pretty =0)
 
 response.binary.test.3 <-data.model.2$response.binary
 response.binary.test.3
 tree.model.pred.3
 
 tree.model.pred.3<-predict (prune.tree.model.3 , data.model.2 ,type ="class")
 table(tree.model.pred.3 ,response.binary.test.3)
 
 # no cortamos el árbol porque al usar cualquier número de nodos menor a 12, se obtiene un error de clasificación mayor, no menor
 
 # ---------------------------------------------------------------------------------
 
 
