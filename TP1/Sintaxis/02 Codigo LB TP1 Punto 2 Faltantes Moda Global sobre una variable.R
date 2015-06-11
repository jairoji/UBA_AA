setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/UBA_AA/TP1/Datasets/")

library(RWeka)
library(ggplot2)
library(reshape)
library(beepr)
library(xlsx)

base.final = read.arff("Base_Final.arff")
base.final = as.data.frame(base.final)

set.seed(NULL)

Confidence = seq(0.05, 0.5, by = 0.05)
Faltantes = seq(from = 0, to = 85, by = 2.5)/100
seeds = seq(length.out = length(Faltantes), from = 40, by = 1500)

set.seed(12345)

base.final = base.final[,-which(names(base.final)=="IDENPA")]


muestra = sample(1:nrow(base.final), size = nrow(base.final)*.8, replace = F)

base.final.training = as.data.frame(base.final[muestra,])
base.final.test = as.data.frame(base.final[-muestra,])


####
#Sobre una variable - Global
####

P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
info.confidence2 = matrix(ncol = 6)
colnames(info.confidence2) = c("Faltantes", "Confidence", "Tama?o", "Hojas", "Entrenamiento", "Test")
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

ptm <- proc.time()
for(j in 2:length(Faltantes)){
  base.final.training = base.final.training1
  set.seed(seeds[j-1])
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
proc.time() - ptm
beep(8)

setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/UBA_AA/TP1/Resultados/")

####
#Sobre una variable - Clase
####

set.seed(12345)

muestra = sample(1:nrow(base.final), size = nrow(base.final)*.8, replace = F)

base.final.training = as.data.frame(base.final[muestra,])
base.final.test = as.data.frame(base.final[-muestra,])

set.seed(NULL)
P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
info.confidence3 = matrix(ncol = 6)
colnames(info.confidence3) = c("Faltantes", "Confidence", "Tama?o", "Hojas", "Entrenamiento", "Test")
info.confidence3[1,] = c(Faltantes[1], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])

date()
for(i in 2:length(Confidence)){
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence3 = rbind(info.confidence3, c(Faltantes[1], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
}
date()

var.imputar = which(colnames(base.final.training) == "P15N")
base.final.training1 = base.final.training

ptm <- proc.time()
for(j in 2:length(Faltantes)){
  base.final.training = base.final.training1
  #set.seed(seeds[j-1])
  muestra1 = sample(1:nrow(base.final.training), size = as.integer((nrow(base.final.training))*Faltantes[j]))
  base.final.training[, var.imputar] = as.character(base.final.training[, var.imputar])
  base.final.training.l1 = subset(base.final.training, P10ST == "Está progresando")
  base.final.training.l2 = subset(base.final.training, P10ST == "No está progresando")
  base.final.training.l1[muestra1, var.imputar] = NA
  base.final.training.l1[which(is.na(base.final.training.l1[,var.imputar])==T), var.imputar] = which.max(table(as.character(base.final.training.l1[, var.imputar])))
  base.final.training.l2[muestra1, var.imputar] = NA
  base.final.training.l2[which(is.na(base.final.training.l2[,var.imputar])==T), var.imputar] = which.max(table(as.character(base.final.training.l2[, var.imputar])))
  base.final.training = rbind(base.final.training.l1, base.final.training.l2)
  base.final.training[, var.imputar] = as.factor(base.final.training[, var.imputar])
  
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence3 = rbind(info.confidence3, c(Faltantes[j], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  
  for(i in 2:length(Confidence)){
    P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
    P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
    info.confidence3 = rbind(info.confidence3, c(Faltantes[j], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  }
  print(paste(j, length(muestra1)))
}
proc.time() - ptm
beep(8)


info.confidence2 = as.data.frame(info.confidence2)
write.xlsx(info.confidence2, "2.1.1Col.Unic.Fil.Unic.Global.xlsx", sheetName = "Variable Unica, Fila Unica")

info.confidence3 = as.data.frame(info.confidence3)
write.xlsx(info.confidence3, "2.1.2Col.Unic.Fil.Unic.Clase.xlsx", sheetName = "Variable Unica, Fila Unica")



info.confidence.melt = melt(info.confidence3[,c(1,2,5,6)], id.vars = c("Confidence", "Faltantes"))
colnames(info.confidence.melt) = c("Confianza", "Faltantes", "Característica", "Porcentaje")
info.confidence.melt[,"Confianza"] = as.factor(info.confidence.melt[,"Confianza"])
Conf.VS.Faltantes = ggplot(data = info.confidence.melt, aes(x = Faltantes, y = Porcentaje, 
                                                        group = interaction(Confianza, Característica), linetype = Característica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes

info.confidence.melt.Entrenamiento = subset(info.confidence.melt, Caracter?stica == "Entrenamiento")
info.confidence.melt.Entrenamiento[,"Caracter?stica"] = droplevels(info.confidence.melt.Entrenamiento[,"Caracter?stica"])

Conf.VS.Faltantes.Entre = ggplot(data = info.confidence.melt.Entrenamiento, aes(x = Faltantes, y = Porcentaje, 
                                                                            group = Confianza, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes.Entre

info.confidence.melt.Test = subset(info.confidence.melt, Caracter?stica == "Test")
info.confidence.melt.Test[,"Caracter?stica"] = droplevels(info.confidence.melt.Test[,"Caracter?stica"])

Conf.VS.Faltantes.Test = ggplot(data = info.confidence.melt.Test, aes(x = Faltantes, y = Porcentaje, 
                                                                  group = Confianza, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes.Test


Conf.VS.Faltantes = ggplot(data = info.confidence.melt, aes(x = Faltantes, y = Porcentaje, 
                                                        group = interaction(Confianza, Caracter?stica), linetype = Caracter?stica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes



info.confidence.melt1 = melt(info.confidence2[,c(1,2,3,4)], id.vars = c("Confidence", "Faltantes"))
colnames(info.confidence.melt1) = c("Confianza", "Faltantes", "Caracter?stica", "N?mero")
info.confidence.melt1[,"Confianza"] = as.factor(info.confidence.melt1[,"Confianza"])
Conf.VS.Tamano = ggplot(data = info.confidence.melt1, aes(x = Faltantes, y = N?mero, 
                                                          group = interaction(Confianza, Caracter?stica), colour = Confianza, linetype = Caracter?stica)) + geom_line(size = 1.2)
Conf.VS.Tamano
