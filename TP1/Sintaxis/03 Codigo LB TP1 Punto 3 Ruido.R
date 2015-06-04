setwd("C:/Users/Ahriman/Google Drive/UBA Aprendizaje autom�tico/TP1/Datasets")

library(RWeka)
library(ggplot2)
library(reshape)

base.final = read.arff("Base_Final.arff")

#base.final = base.final[,c(2:8, 13, 14)]

set.seed(NULL)

Confidence = seq(0.05, 0.5, by = 0.05)
Ruido = seq(from = 0, to = 35, by = 0.5)/100
seeds = seq(length.out = length(Ruido), from = 40, by = 1500)

set.seed(12345)

base.final = base.final[,-which(names(base.final)=="IDENPA")]

muestra = sample(1:nrow(base.final), size = nrow(base.final)*.8, replace = F)

base.final.training = as.data.frame(base.final[muestra,])
base.final.test = as.data.frame(base.final[-muestra,])

P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
info.confidence2 = matrix(ncol = 6)
colnames(info.confidence2) = c("Ruido", "Confidence", "Tama�o", "Hojas", "Entrenamiento", "Test")
info.confidence2[1,] = c(Ruido[1], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])

date()
for(i in 2:length(Confidence)){
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence2 = rbind(info.confidence2, c(Ruido[1], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
}
date()

var.imputar = which(colnames(base.final.training) == "P10ST")

base.final.training1 = base.final.training

date()
for(j in 2:length(Ruido)){
  set.seed(seeds[j-1])
  base.final.training = base.final.training1
  muestra1 = sample(1:nrow(base.final.training), size = as.integer((nrow(base.final.training))*Ruido[j]))
  base.final.training[, var.imputar] = as.character(base.final.training[, var.imputar])
  base.final.training[muestra1, var.imputar] = NA
  for(k in 1:length(muestra1)){
    base.final.training[muestra1[k], var.imputar] = as.character(levels(base.final[,var.imputar])[floor(runif(1, min = 1, max = length(levels(base.final[,var.imputar]))+.1))])
  }
  base.final.training[, var.imputar] = as.factor(base.final.training[, var.imputar])
  
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence2 = rbind(info.confidence2, c(Ruido[j], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  
  for(i in 2:length(Confidence)){
    P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
    P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
    info.confidence2 = rbind(info.confidence2, c(Ruido[j], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  }
  print(paste(j, length(muestra1)))
}

date()

info.confidence2 = as.data.frame(info.confidence2)
info.confidence.melt = melt(info.confidence2[,c(1,2,5,6)], id.vars = c("Confidence", "Ruido"))
colnames(info.confidence.melt) = c("Confianza", "Ruido", "Caracter�stica", "Porcentaje")
info.confidence.melt[,"Confianza"] = as.factor(info.confidence.melt[,"Confianza"])
Conf.VS.Ruido = ggplot(data = info.confidence.melt, aes(x = Ruido, y = Porcentaje, 
                                                        group = interaction(Confianza, Caracter�stica), linetype = Caracter�stica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Ruido

info.confidence.melt.Entrenamiento = subset(info.confidence.melt, Caracter�stica == "Entrenamiento")
info.confidence.melt.Entrenamiento[,"Caracter�stica"] = droplevels(info.confidence.melt.Entrenamiento[,"Caracter�stica"])

Conf.VS.Ruido.Entre = ggplot(data = info.confidence.melt.Entrenamiento, aes(x = Ruido, y = Porcentaje, 
                                                              group = Confianza, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Ruido.Entre

info.confidence.melt.Test = subset(info.confidence.melt, Caracter�stica == "Test")
info.confidence.melt.Test[,"Caracter�stica"] = droplevels(info.confidence.melt.Test[,"Caracter�stica"])

Conf.VS.Ruido.Test = ggplot(data = info.confidence.melt.Test, aes(x = Ruido, y = Porcentaje, 
                                                                  group = Confianza, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Ruido.Test


Conf.VS.Ruido = ggplot(data = info.confidence.melt, aes(x = Ruido, y = Porcentaje, 
                                                        group = interaction(Confianza, Caracter�stica), linetype = Caracter�stica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Ruido



info.confidence.melt1 = melt(info.confidence2[,c(1,2,3,4)], id.vars = c("Confidence", "Ruido"))
colnames(info.confidence.melt1) = c("Confianza", "Ruido", "Caracter�stica", "N�mero")
info.confidence.melt1[,"Confianza"] = as.factor(info.confidence.melt1[,"Confianza"])
Conf.VS.Tamano = ggplot(data = info.confidence.melt1, aes(x = Ruido, y = N�mero, 
                                                        group = interaction(Confianza, Caracter�stica), colour = Confianza, linetype = Caracter�stica)) + geom_line(size = 1.2)
Conf.VS.Tamano