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
 threshold<-as.numeric(quantile(data[,response],0.5,na.rm=TRUE))
 data$response.binary<-as.factor(ifelse(data[,response]>threshold,"High","Low"))
summary(data$response.binary)
# quantile(data[,response],0.5,na.rm=TRUE)

summary(data[,response])
plot(density(data[,response]))


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
 train<-sample (1: nrow(data.model), 9) # para hacer el conj de entrenam agarro una muestra aleatoria de 100 datos de entre los renglones 1 y 180
 data.model.test<-data.model[-train ,] # para el de prueba se usan los 80 registros que no se usaron para el conjunto de entrenamiento
 response.binary.test<-data.model$response.binary[-train ] # vemos los valores que toman los 80 datos de prueba en la columna "response.binary"
 tree.model <- tree(model,data.model) # se usan el modelo, la base de datos y el subconjunto
 tree.model.pred<-predict(tree.model ,data.model.test ,type ="class") # probamos qué tan buen modelo es dándole el árbol estimado, el conjunto de prueba y type
 table(tree.model.pred ,response.binary.test)
 plot(tree.model)
 text(tree.model  ,pretty =0)
# So this tree has an error rate of (5+4)/80
 
 
 data.test <- data.test[complete.cases(data.test),]
 summary(data.test)
 dim(data.test)
 
 train<- data # para hacer el conj de entrenam agarro una muestra aleatoria de 100 datos de entre los renglones 1 y 180
 data.model.test<-data.model.2[-train ,] # para el de prueba se usan los 80 registros que no se usaron para el conjunto de entrenamiento
 response.binary.test<-data.model.2$response.binary # vemos los valores que toman los 80 datos de prueba en la columna "response.binary"
 tree.model <- tree(model,data.model.2) # se usan el modelo, la base de datos y el subconjunto
 tree.model.pred<-predict(tree.model ,data.test ,type ="class") # probamos qué tan buen modelo es dándole el árbol estimado, el conjunto de prueba y type
 table(tree.model.pred ,response.binary.test)
 plot(tree.model)
 text(tree.model  ,pretty =0)
 
 
 # model test ----------

 
 threshold.2 <-as.numeric(quantile(data.test[,response],0.5,na.rm=TRUE))
 data.test$response.binary<-as.factor(ifelse(data.test[,response]>threshold,"High","Low"))
 summary(data.test$response.binary)
 
 data.model.2 <- data.test[,c("response.binary",predictors)]
 data.model.2 <- data.model.2[complete.cases(data.model.2),]
 summary(data.model.2)
 dim(data.model.2)
 summary(data.model.2$response.binary)
 dim(tree.model.pred2)


 set.seed (55555)
 tree.model.2 <- tree(model,data.model.2) # se usan el modelo, la base de datos y el subconjunto
 tree.model.pred2 <-predict(tree.model.2 ,data.model.2 ,type ="class") # probamos qué tan buen modelo es dándole el árbol estimado, el conjunto de prueba y type
 table(tree.model.pred2 ,tree.model.2)
 plot(tree.model)
 text(tree.model  ,pretty =0)
 
 
 
 # ---------------------

#We can use tree prunning to get a better classification tree
 set.seed (55555)
 library(tree)
 cv.data.model<-cv.tree(tree.model ,FUN=prune.misclass)
 cv.data.model

# dev corresponds to the cross-validation error rate in this instance, which is then the best tree?

#we can use prune.misclass() to obtain the best tree
  prune.tree.model <- prune.misclass (tree.model ,best =4)
  plot(prune.tree.model )
  text(prune.tree.model ,pretty =0)

#how good is the prune tree ? proceso de validación
  tree.pred<-predict (prune.tree.model , data.model.test ,type ="class")
  table(tree.pred ,response.binary.test)

# So this tree has an error rate of (6+3)/80


#========================
# Approach 2: Regression tree
#========================

#single best
 library (MASS)
 set.seed (55555)
 train <- sample (1: nrow(data.model), 100)
 Rtree.data.model <-tree(model,data.model ,subset =train)
 summary (Rtree.data.model)
 plot(Rtree.data.model  )
 text(Rtree.data.model  ,pretty =0)

#prune tree
 cv.Rtree.data.model <- cv.tree(Rtree.data.model )
 cv.Rtree.data.model

#which is the best tree?

 prune.Rtree.data.model <- prune.misclass(Rtree.data.model  ,best =3)
 plot(prune.Rtree.data.model )
 text(prune.Rtree.data.model ,pretty =0)

 #========================
 # Approach 3: Random Forest
 #========================
#install.packages("randomForest")
 library(randomForest)
 set.seed (55555)
 rf.data.model <- randomForest(model,
                              data=data.model ,
                              subset =train ,
                              mtry=round(length(predictors)^0.5),
                              importance =TRUE
                              )
 rf.data.model

#base on many different trees, which are the most important drivers
 importance (rf.data.model)
 varImpPlot (rf.data.model )
 # entre más alto, más importante
 
 
