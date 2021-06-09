
dir.data <- "/Users/regisheeran/Desktop/"
data <- read.csv(paste0(dir.data,"ModelData.csv"))
summary(data)

bad.vars <- sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars <- names(bad.vars[bad.vars>0.50])

ids <- c("iso_code3","country")
response <- "EN.ATM.CO2E.PC"

#Predictors 1 es la combinación de NDC (cosas que si puede cambair un gobierno) y factores sociodemográficos según lo que había
#Predictos 2 es solo NDC
#Predictors 3 es solo sociodemográficos
predictors1 <- subset(colnames(data), !(colnames(data)%in%c(ids,response,bad.vars, "a_agriculture", "a_coastal_zone", "a_energy", "A_Im_Finan", "a_tourism", "m_agriculture","m_industries", "a_cross_cutting_area", "a_drm",  "a_education", "a_environment" , "a_health" , "A_Im_CapBul", "a_water",  "m_economy.wide", "m_energy",  "m_lulucf",  "M_PL7",  "migration_and_displacement", "AG.LND.FRST.K2", "AG.SRF.TOTL.K2"  )))
predictors3 <- subset(colnames(data), !(colnames(data)%in%c(ids,response,bad.vars,"a_coastal_zone", "a_cross_cutting_area", "a_drm", "a_education", "a_energy",  "a_environment", "a_health", "A_Im_CapBul", "A_Im_Finan", "A_Im_TecTran","a_lulucf", "a_social_development" , "a_tourism",  "a_transport", "a_urban", "a_water", "m_agriculture", "m_buildings",  "m_economy.wide", "m_energy", "m_industries", "m_lulucf", "M_PL7" ,  "m_transport", "m_waste","migration_and_displacement" )))
predictors2 <- subset(colnames(data), !colnames(data)%in%c(ids,response,bad.vars, "AG.LND.FRST.K2", "AG.SRF.TOTL.K2", "BM.TRF.PWKR.CD.DT", "BX.KLT.DINV.CD.WD", "DT.DOD.DECT.CD", "DT.ODA.ODAT.PC.ZS", "DT.TDS.DECT.GN.ZS", "GC.REV.XGRT.GD.ZS", "IC.REG.PROC", "IT.CEL.SETS.P2",  "MS.MIL.XPND.GD.ZS" ,  "NE.EXP.GNFS.ZS", "NE.GDI.TOTL.ZS", "NE.IMP.GNFS.ZS", "NV.AGR.TOTL.ZS",  "NV.IND.TOTL.ZS" ,"NY.GDP.DEFL.KD.ZG", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG","NY.GNP.ATLS.CD","NY.GNP.MKTP.PP.CD","NY.GNP.PCAP.CD", "NY.GNP.PCAP.PP.CD",  "SE.ENR.PRSC.FM.ZS" ,  "SE.PRM.CMPT.ZS", "SE.SEC.ENRR" ,"SH.DYN.AIDS.ZS",  "SH.DYN.MORT",  "SH.IMM.MEAS",  "SH.STA.BRTC.ZS",  "SI.DST.FRST.20",  "SP.ADO.TFRT", "SP.DYN.LE00.IN",  "SP.DYN.TFRT.IN", "SP.POP.GROW",  "SP.POP.TOTL",  "TG.VAL.TOTL.GD.ZS", "TT.PRI.MRCH.XD.WD", "TX.VAL.TECH.MF.ZS"))

length(predictors1)
length(predictors2)
length(predictors3)


#before we model, how correlated is our data  para el LM 1
cor.table1<-data.frame(cor(data[,predictors1], use = "complete.obs"))
bad.cors1<-sapply(cor.table1, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.cors1<-names(bad.cors1[bad.cors1>0.98])
cor.table1<-cor.table1[,subset(colnames(cor.table1),!(colnames(cor.table1)%in%bad.cors1))]
cor.table1<-cor.table1[subset(rownames(cor.table1),!(rownames(cor.table1)%in%bad.cors1)),]
cor.table1<-data.frame(apply(cor.table1,c(1,2),function(x){ifelse(abs(x)>0.5,1,0)}))
bad.cors1.1<-sapply(cor.table1,mean)
bad.cors1.1<-names(bad.cors1.1[bad.cors1.1>0.40])
predictors1<-subset(predictors1,!(predictors1%in%bad.cors1.1))

#before we model, how correlated is our data  para el LM 2
cor.table2<-data.frame(cor(data[,predictors2], use = "complete.obs"))
bad.cors2<-sapply(cor.table2, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.cors2<-names(bad.cors2[bad.cors2>0.98])
cor.table2<-cor.table2[,subset(colnames(cor.table2),!(colnames(cor.table2)%in%bad.cors2))]
cor.table2<-cor.table2[subset(rownames(cor.table2),!(rownames(cor.table2)%in%bad.cors2)),]
cor.table2<-data.frame(apply(cor.table2,c(1,2),function(x){ifelse(abs(x)>0.5,1,0)}))
bad.cors2.1<-sapply(cor.table2,mean)
bad.cors2.1<-names(bad.cors2.1[bad.cors2.1>0.40])
predictors2<-subset(predictors2,!(predictors2%in%bad.cors2.1))

#before we model, how correlated is our data  para el LM 3
cor.table3<-data.frame(cor(data[,predictors3], use = "complete.obs"))
bad.cors3<-sapply(cor.table3, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.cors3<-names(bad.cors3[bad.cors3>0.98])
cor.table3<-cor.table3[,subset(colnames(cor.table3),!(colnames(cor.table3)%in%bad.cors3))]
cor.table3<-cor.table3[subset(rownames(cor.table3),!(rownames(cor.table3)%in%bad.cors3)),]
cor.table3<-data.frame(apply(cor.table3,c(1,2),function(x){ifelse(abs(x)>0.5,1,0)}))
bad.cors3.1<-sapply(cor.table3,mean)
bad.cors3.1<-names(bad.cors3.1[bad.cors3.1>0.40])
predictors3<-subset(predictors3,!(predictors3%in%bad.cors3.1))


#Estimar Modelo 1




#Estimar Modelo 2



#Estimar Modelo 3
