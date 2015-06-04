setwd("C:/Users/Ahriman/Google Drive/UBA Aprendizaje automático/TP1/Datasets")

library(RWeka)
library(ggplot2)
library(reshape)

base.final = read.arff("Base_Final.arff")
base.final = as.data.frame(base.final)

#base.final = base.final[,c(2:8, 13, 14)]

set.seed(NULL)

Confidence = seq(0.05, 0.5, by = 0.05)
Faltantes = seq(from = 0, to = 85, by = 2.5)/100
#seeds = seq(length.out = length(Faltantes), from = 40, by = 1500)

#set.seed(12345)

base.final = base.final[,-which(names(base.final)=="IDENPA")]

muestra = sample(1:nrow(base.final), size = nrow(base.final)*.8, replace = F)

base.final.training = as.data.frame(base.final[muestra,])
base.final.test = as.data.frame(base.final[-muestra,])

#names(which.max(table(as.character(base.final.training[,2]))))

P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
info.confidence2 = matrix(ncol = 6)
colnames(info.confidence2) = c("Faltantes", "Confidence", "Tamaño", "Hojas", "Entrenamiento", "Test")
info.confidence2[1,] = c(Faltantes[1], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])

date()
for(i in 2:length(Confidence)){
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence2 = rbind(info.confidence2, c(Faltantes[1], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
}
  date()

var.imputar = which(colnames(base.final.training) == "P15N")
base.final.training1 = base.final.training

date()
for(j in 2:length(Faltantes)){
  base.final.training = base.final.training1
  #set.seed(seeds[j-1])
  muestra1 = sample(1:nrow(base.final.training), size = as.integer((nrow(base.final.training))*Faltantes[j]))
  base.final.training[, var.imputar] = as.character(base.final.training[, var.imputar])
  base.final.training[muestra1, var.imputar] = NA
  base.final.training[muestra1, var.imputar] = which.max(table(as.character(base.final.training[, var.imputar])))
  base.final.training[, var.imputar] = as.factor(base.final.training[, var.imputar])
  
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence2 = rbind(info.confidence2, c(Faltantes[j], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  
  for(i in 2:length(Confidence)){
    P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
    P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
    info.confidence2 = rbind(info.confidence2, c(Faltantes[j], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  }
  print(paste(j, length(muestra1)))
}

date()

names(P10ST.J48$classifier)
P10ST.J48$classifier$toSummaryString()
P10ST.J48$levels

info.confidence2 = as.data.frame(info.confidence2)
info.confidence.melt = melt(info.confidence2[,c(1,2,5,6)], id.vars = c("Confidence", "Faltantes"))
colnames(info.confidence.melt) = c("Confianza", "Faltantes", "Característica", "Porcentaje")
info.confidence.melt[,"Confianza"] = as.factor(info.confidence.melt[,"Confianza"])
Conf.VS.Faltantes = ggplot(data = info.confidence.melt, aes(x = Faltantes, y = Porcentaje, 
                                                        group = interaction(Confianza, Característica), linetype = Característica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes

info.confidence.melt.Entrenamiento = subset(info.confidence.melt, Característica == "Entrenamiento")
info.confidence.melt.Entrenamiento[,"Característica"] = droplevels(info.confidence.melt.Entrenamiento[,"Característica"])

Conf.VS.Faltantes.Entre = ggplot(data = info.confidence.melt.Entrenamiento, aes(x = Faltantes, y = Porcentaje, 
                                                                            group = Confianza, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes.Entre

info.confidence.melt.Test = subset(info.confidence.melt, Característica == "Test")
info.confidence.melt.Test[,"Característica"] = droplevels(info.confidence.melt.Test[,"Característica"])

Conf.VS.Faltantes.Test = ggplot(data = info.confidence.melt.Test, aes(x = Faltantes, y = Porcentaje, 
                                                                  group = Confianza, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes.Test


Conf.VS.Faltantes = ggplot(data = info.confidence.melt, aes(x = Faltantes, y = Porcentaje, 
                                                        group = interaction(Confianza, Característica), linetype = Característica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes



info.confidence.melt1 = melt(info.confidence2[,c(1,2,3,4)], id.vars = c("Confidence", "Faltantes"))
colnames(info.confidence.melt1) = c("Confianza", "Faltantes", "Característica", "Número")
info.confidence.melt1[,"Confianza"] = as.factor(info.confidence.melt1[,"Confianza"])
Conf.VS.Tamano = ggplot(data = info.confidence.melt1, aes(x = Faltantes, y = Número, 
                                                          group = interaction(Confianza, Característica), colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
Conf.VS.Tamano
