
dir.data <- "/Users/regisheeran/Desktop/"
data <- read.csv(paste0(dir.data,"ModelData.csv"))
summary(data)

bad.vars <- sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars <- names(bad.vars[bad.vars>0.50])

ids <- c("iso_code3","country")
response <- "EN.ATM.CO2E.PC"

predictors <- subset(colnames(data), !(colnames(data)%in%c(ids,response,bad.vars))) 

cor.table<-data.frame(cor(data[,predictors], use = "complete.obs"))
bad.cors<-sapply(cor.table, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.cors<-names(bad.cors[bad.cors>0.98])
cor.table<-cor.table[,subset(colnames(cor.table),!(colnames(cor.table)%in%bad.cors))]
cor.table<-cor.table[subset(rownames(cor.table),!(rownames(cor.table)%in%bad.cors)),]
cor.table<-data.frame(apply(cor.table,c(1,2),function(x){ifelse(abs(x)>0.5,1,0)}))
bad.cors2<-sapply(cor.table,mean)
bad.cors2<-names(bad.cors2[bad.cors2>0.40])
predictors<-subset(predictors,!(predictors%in%bad.cors2))
data.model<- data[,c(response, predictors)]
model <- as.formula(paste0(response,"~", paste(predictors,collapse="x")))
full.model <- lm(model, data=data.model)