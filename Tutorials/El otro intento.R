library(readr)
dir.data <- "/Users/regisheeran/Desktop/"
data <- read.csv(paste0(dir.data,"ModelData2.csv"))

#let's look at the data first,
summary(data)


#remove columns with only NA
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.03])
bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#let's select a response of interest
#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
response<-("EN.ATM.CO2E.PC")


predictors<-subset(colnames(data), colnames(data)%in%c("BX.KLT.DINV.CD.WD", "SP.POP.TOT2L", "a_energy", "a_urban", "a_transport", "a_social_development", "a_education", "a_environment", "m_waste", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG", "SP.POP.GROW"))
#estimate full model
data.model<-data[,c(response,predictors)]
#clean the data database
data.model<-data.model[complete.cases(data.model),]
plot(density(data.model$EN.ATM.CO2E.PC))

#education
plot(density(data.model$a_education))#dummy menor que 1 
education<- log(data.model$a_education)
plot(density(education)) #comprobar que sea campana

#energy
plot(density(data.model$a_energy)) #hacerla log (reconvertir 0s) o dummy de 0 
energy<- log(data.model$a_energy) #NOFUNCIONö
plot(density(energy))

#social_deevelopment
plot(density(data.model$a_social_development)) 
social_development<-  (data.model$a_social_development) #NoFuncionó
plot(social_development)

#Bx.klt
plot(density(data.model$BX.KLT.DINV.CD.WD))
Bx.klt<- (data.model$BX.KLT.DINV.CD.WD) #NoFuncionó
plot(Bx.klt)

#SP.POP
plot(density(data.model$SP.POP.GROW))
SP.POP<-(data.model$SP.POP.GROW) #NoFuncionó
plot(SP.POP)


#Urban
plot(density(data.model$a_urban))
Urban<-(data.model$a_urban-mean(data.model$a_urban))/sd(data.model$a_urban)
plot(Urban)

#Transport
plot(density(data.model$a_transport))
Transport<- (data.model$a_transport) #NoFuncionó
plot(Transport)

#Envi
plot(density(data.model$a_environment))
Envi<- log(data.model$a_environment) #NoFuncionó 
plot(Envi)

#waste
plot(density(data.model$m_waste))
waste<- log(data.model$m_waste) #nofuncionó
plot(waste)

#ny.gdp
plot(density(data.model$NY.GDP.MKTP.CD))
ny.gdp<- log(data.model$NY.GDP.MKTP.CD) #nop
plot(ny.gdp)

#ny.zg
plot(density(data.model$NY.GDP.MKTP.KD.ZG))
ny.zg<- (data.model$NY.GDP.MKTP.KD.ZG) #nop
plot(ny.zg)

colnames(data) <- c("EN.ATM.CO2E.PC","education", "social_development", "energy","Bx.klt", "SP.POP", "Urban", "Transport", "Envi", "waste", "ny.gdp","ny.zg")
predictors<-subset(colnames(data), colnames(data)%in%c("education", "social_development", "energy","Bx.klt", "SP.POP", "Urban", "Transport", "Envi", "waste", "ny.gdp","ny.zg"))

predictors
model<-as.formula(paste(response,"~",paste(predictors,collapse="+")))
data.model<-data[,c(response,predictors)]

#comienzan errores
data.model<-data.model[complete.cases(data.model),]

as.numeric("EN.ATM.CO2E.PC","education", "social_development", "energy","Bx.klt", "SP.POP", "Urban", "Transport", "Envi", "waste", "ny.gdp","ny.zg", na.action=na.omit)
call(data.model)
model
plot(model)

full.model <- glm(model, data = data.model, na.action=na.omit)

full.model
plot(full.model)
summary(full.model)

#now let's select various smaller models
#install.packages("leaps")



library(leaps)


#first let's divide the sample into a test and a a train set
set.seed (55555)
train <- sample (c(TRUE ,FALSE), nrow(data.model ),rep=TRUE)
test  <- (!train )

#define maximum length of the model
max.vars<-8

#let's do full search
regfit.best <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T) #you can choose how large you want the search to be

#let's do forward stepwise selection
regfit.fwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "forward") #you can choose how large you want the search to be

#let's do backard stepwise selection
regfit.bwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "backward") #you can choose how large you want the search to be

#now how do we select which model is best

predict.regsubsets <-function (object, model ,newdata ,id ){
   object<-regfit.best
  newdata<-data.model[test ,]
   id<-4
  form<-model
  options(na.action='na.pass')
  mat<-model.matrix (form,newdata )
  coefi<-coef(object ,id=id)
  xvars<-names (coefi )
  pred<-mat[,xvars ]%*% coefi
  val.errors<- mean((newdata[,response]-pred)^2,na.rm=TRUE)
  val.errors
}

#now estimate test error for the different versions of the models
#best subset
cv.best<-data.frame(subset.type="best",
                    nvars=1,
                    test.mse=predict.regsubsets(regfit.best,model,data.model[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="best",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.best,model,data.model[test ,],i))
  cv.best<-rbind(cv.best,pivot)
  
}

#best model
subset(cv.best,test.mse==min(test.mse))
#actual model
coef(regfit.best ,subset(cv.best,test.mse==min(test.mse))$nvars)

#forward method
cv.fwd<-data.frame(subset.type="fwd",
                   nvars=1,
                   test.mse=predict.regsubsets(regfit.fwd,model,data.model[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="fwd",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.fwd,model,data.model[test ,],i))
  cv.fwd<-rbind(cv.fwd,pivot)
  
}

#best model
subset(cv.fwd,test.mse==min(test.mse))
#actual model
coef(regfit.fwd ,subset(cv.fwd,test.mse==min(test.mse))$nvars)

#
#backward method
cv.bwd<-data.frame(subset.type="bwd",
                   nvars=1,
                   test.mse=predict.regsubsets(regfit.bwd,model,data.model[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="bwd",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.bwd,model,data.model[test ,],i))
  cv.bwd<-rbind(cv.bwd,pivot)
  
}

#best model
subset(cv.bwd,test.mse==min(test.mse))
#actual model
coef(regfit.bwd ,subset(cv.bwd,test.mse==min(test.mse))$nvars)



