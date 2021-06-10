#This script uses different methods to fit a classification tree to our data set
 dir.data<-"C:/Users/cinth/Documents/BCDD/Evidencia1/Data/Model/"
 dataO<-read.csv(paste0(dir.data,"ModelData.csv"))

#let's look at the data first,
  summary(data)

#remove columns with only NA
 bad.vars<-sapply(dataO, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
 bad.vars<-names(bad.vars[bad.vars>0.03])
 bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#let's select a response of interest
#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
  response<-"EN.ATM.CO2E.PC"
  #  predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))
  predictors<-subset(colnames(dataO), colnames(dataO)%in%c("BX.KLT.DINV.CD.WD", "SP.POP.TOT2L", "a_energy", "a_urban", "a_transport", "a_social_development", "a_education", "a_environment", "m_waste", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG", "SP.POP.GROW"))

data <- dataO[c(4, 150, 6, 8, 19, 25, 18, 24, 26, 32, 38, 5, 138, 78, 77, 88, 95, 124, 112, 108, 118, 127, 134, 139, 136, 148, 167, 192, 171, 176, 178, 183, 33),c(predictors,response)] 
nic<- c(4, 150, 6, 8, 19, 25, 18, 24, 26, 32, 38, 5, 138, 78, 77, 88, 95, 124, 112, 108, 118, 127, 134, 139, 136, 148, 167, 192, 171, 176, 178, 183, 33)

data.test <- dataO[!seq(1,nrow(dataO)) %in% nic,c(predictors,response)] 
data.test

dim(data)  
predictors
View(data)

# Since we are analyzing this as a classification problem
#let's look at how this response is distributed
  summary(data[,response])
  data <- data[complete.cases(data),]
  dim(data)
  View(data)

# now let's partition the response of interest into two groups
#focus on the high emissions per capita nations

  # PARA EL CONJUNTO DE ENTRENAMIENTO (31 PAISES)
threshold<-as.numeric(quantile(data[,response],0.5,na.rm=TRUE))
data$response.binary<-as.factor(ifelse(data[,response]>threshold,"High","Low"))
summary(data$response.binary)
# quantile(data[,response],0.5,na.rm=TRUE)

# PARA EL CONJUNTO DE PRUEBA (150 PAISES)
threshold.2 <-as.numeric(quantile(data.test[,response],0.5,na.rm=TRUE))
data.test$response.binary<-as.factor(ifelse(data.test[,response]>threshold,"High","Low"))
summary(data.test$response.binary)

# summary(data[,response])
# plot(density(data[,response]))

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
 
 # PARA EL CONJUNTO DE PRUEBA
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


#========================
# Approach 1: Classification tree
#========================
# install.packages("tree")
 library(tree)
 set.seed (55555)
 tree.model <- tree(model,data.model) # se usan el modelo, la base de datos y el subconjunto
 tree.model
 response.binary.model<-data.model$response.binary # vemos los valores que toman los 80 datos de prueba en la columna "response.binary"
 
 tree.model.pred<-predict(tree.model ,data.model,type ="class") # probamos qué tan buen modelo es dándole el árbol estimado, el conjunto de prueba y type
 
 #response.binary.test<-data.model.2$response.binary # vemos los valores que toman los 80 datos de prueba en la columna "response.binary"
 #tree.model <- tree(model,data.model.2) # se usan el modelo, la base de datos y el subconjunto
 #tree.model.pred<-predict(tree.model ,data.test ,type ="class") # probamos qué tan buen modelo es dándole el árbol estimado, el conjunto de prueba y type
 
 
 table(tree.model.pred ,response.binary.model)
 plot(tree.model)
 text(tree.model,pretty =0)
# So this tree has an error rate of (5+4)/80
 
 response.binary.test<-data.model.2$response.binary
 response.binary.test
 tree.model.pred
 # usar sample para que me de 31 observaciones de las 150
 
 test <- sample(response.binary.test, 31, replace = FALSE)
 
# MATRIZ DE CONFUSIÓN PREDICCIÓN Y PRUEBA
  table(tree.model.pred, test)
 
 
 # model test ----------

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

 #RANDOM FOREST
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
 
 
 # ---------------------


 #========================
 # Approach 3: Random Forest
 #========================
#install.packages("randomForest")
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
 # entre más alto, más importante
 
 
