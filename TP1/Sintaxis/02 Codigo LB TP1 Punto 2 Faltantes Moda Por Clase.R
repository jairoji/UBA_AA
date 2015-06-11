Imputar.Moda.Fun = function(X, trim = 0){
  if(length(is.na(X))==0){
    print("Variable sin faltantes")
    return(X)
  }
  if(is.numeric(X)){
    X[which(is.na(X))] = mean(X, trim = trim, na.rm = T)
    return(X)
  }
  else{
    X[which(is.na(X))] = levels(X)[which.max(table(X))]
    return(X)
  }
}

Imputar.Moda.Fun.Factores = function(A){
  A1 = A
  A1 = as.data.frame(sapply(A, Imputar.Moda.Fun))
  for(i in 1:ncol(A1)){
    if(is.factor(A[,i])){
      A1[,i] = as.factor(A1[,i])
      levels(A1[,i]) = levels(A[,i])
    }
    else
      next
  }
  return(A1)
}

Inducir_faltantes = function(X, porc, tipo = "Unico"){
  if(tipo == "Unico"){
    sample.faltantes = sample(1:nrow(X), size = ceiling(nrow(X)*porc))
    for(i in 1:length(sample.faltantes)){
      X[sample.faltantes[i], floor(runif(1, min = 2, max = ncol(X)+1))] = NA
    }
    return(X)
  }
  if(tipo == "Multiple"){
    total.falt = ceiling(nrow(X)*(ncol(X)-1)*porc)
    while(length(which(is.na(X))) < total.falt){
      set.seed(NULL)
      numero = runif(1, 1, ncol(X)-1)
      sample.faltantes = sample(1:nrow(X), size = 1)
      X[sample.faltantes, unique(floor(runif(numero, 2, ncol(X))))] = NA
    }
    return(X)
  }
}

setwd("/run/media/jjimenez/Stuff/MDMKD/Primer cuatrimestre/UBA_AA/TP1/Datasets/")

library(RWeka)
library(ggplot2)
library(reshape)
library(beepr)
library(scales)

base.final = read.arff("Base_Final.arff")
base.final = as.data.frame(base.final)

Confidence = seq(0.05, 0.5, by = 0.05)
Faltantes = seq(from = 0, to = 85, by = 2.5)/100
seeds = seq(length.out = length(Faltantes), from = 40, by = 1500)

set.seed(12345)

base.final = base.final[,-which(names(base.final)=="IDENPA")]

base.final = base.final[, c("P10ST", "S1NICC7", "S15", "S16", "S17", "S14", "S28D", "P49ST.A", "P76ST", 
                            "P1ST", "P3ST.A", "P3ST.B", "P4ST", "P15N", "P19ST", 
                            "P28NE", "P65ST", "P6ST", "P12ST", "P13ST")]

muestra = sample(1:nrow(base.final), size = nrow(base.final)*.8, replace = F)

base.final.training = as.data.frame(base.final[muestra,])
base.final.test = as.data.frame(base.final[-muestra,])


##Unico
info.confidence.unico = NULL
P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
info.confidence.unico = matrix(ncol = 6)
colnames(info.confidence.unico) = c("Faltantes", "Confidence", "Tamaño", "Hojas", "Entrenamiento", "Test")
info.confidence.unico[1,] = c(Faltantes[1], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])

ptm <- proc.time()
for(i in 2:length(Confidence)){
  P10ST.J48 = NULL
  P10ST.J48.e = NULL
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence.unico = rbind(info.confidence.unico, c(Faltantes[1], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
}
proc.time() - ptm

base.final.training1 = base.final.training

ptm <- proc.time()
for(j in 2:length(Faltantes)){
  base.final.training = base.final.training1
  set.seed(seeds[j-1])
  base.final.training = Inducir_faltantes(base.final.training, porc = Faltantes[j], "Unico")
  base.final.training.lv1 = subset(base.final.training, P10ST == "Está progresando") 
  base.final.training.lv2 = subset(base.final.training, P10ST == "No está progresando")
  base.final.training.lv1 = base.final.training.lv1[,-1]
  base.final.training.lv2 = base.final.training.lv2[,-1]
  base.final.training.lv1 = Imputar.Moda.Fun.Factores(base.final.training.lv1)
  base.final.training.lv2 = Imputar.Moda.Fun.Factores(base.final.training.lv2)
  base.final.training.lv1 = cbind(rep("Está progresando", nrow(base.final.training.lv1)), base.final.training.lv1)
  base.final.training.lv2 = cbind(rep("No está progresando", nrow(base.final.training.lv2)), base.final.training.lv2)
  names(base.final.training.lv2) = c("P10ST", "S1NICC7", "S15", "S16", "S17", "S14", "S28D", "P49ST.A", "P76ST", 
                                     "P1ST", "P3ST.A", "P3ST.B", "P4ST", "P15N", "P19ST", "P28NE", 
                                     "P65ST", "P6ST", "P12ST", "P13ST")
  names(base.final.training.lv1) = c("P10ST", "S1NICC7", "S15", "S16", "S17", "S14", "S28D", "P49ST.A", "P76ST", 
                                     "P1ST", "P3ST.A", "P3ST.B", "P4ST", "P15N", "P19ST", "P28NE", 
                                     "P65ST", "P6ST", "P12ST", "P13ST")
  base.final.training = rbind(base.final.training.lv1, base.final.training.lv2)
  
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence.unico = rbind(info.confidence.unico, c(Faltantes[j], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  
  for(i in 2:length(Confidence)){
    P10ST.J48 = NULL
    P10ST.J48.e = NULL
    P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
    P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
    info.confidence.unico = rbind(info.confidence.unico, c(Faltantes[j], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  }
  print(j)
}
proc.time() - ptm
beep(8)



##Multiple
info.confidence.multiple = NULL
P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
info.confidence.multiple = matrix(ncol = 6)
colnames(info.confidence.multiple) = c("Faltantes", "Confidence", "Tamaño", "Hojas", "Entrenamiento", "Test")
info.confidence.multiple[1,] = c(Faltantes[1], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])

ptm <- proc.time()
for(i in 2:length(Confidence)){
  P10ST.J48 = NULL
  P10ST.J48.e = NULL
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence.multiple = rbind(info.confidence.multiple, c(Faltantes[1], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
}
proc.time() - ptm

base.final.training1 = base.final.training

ptm <- proc.time()
for(j in 2:length(Faltantes)){
  base.final.training = base.final.training1
  set.seed(seeds[j-1])
  base.final.training = Inducir_faltantes(base.final.training, porc = Faltantes[j], "Multiple")
  print(length(which(is.na(base.final.training))))
  base.final.training.lv1 = subset(base.final.training, P10ST == "Está progresando") 
  base.final.training.lv2 = subset(base.final.training, P10ST == "No está progresando")
  base.final.training.lv1 = base.final.training.lv1[,-1]
  base.final.training.lv2 = base.final.training.lv2[,-1]
  base.final.training.lv1 = Imputar.Moda.Fun.Factores(base.final.training.lv1)
  base.final.training.lv2 = Imputar.Moda.Fun.Factores(base.final.training.lv2)
  base.final.training.lv1 = cbind(rep("Está progresando", nrow(base.final.training.lv1)), base.final.training.lv1)
  base.final.training.lv2 = cbind(rep("No está progresando", nrow(base.final.training.lv2)), base.final.training.lv2)
  names(base.final.training.lv2) = c("P10ST", "S1NICC7", "S15", "S16", "S17", "S14", "S28D", "P49ST.A", "P76ST", 
                                     "P1ST", "P3ST.A", "P3ST.B", "P4ST", "P15N", "P19ST", "P28NE", 
                                     "P65ST", "P6ST", "P12ST", "P13ST")
  names(base.final.training.lv1) = c("P10ST", "S1NICC7", "S15", "S16", "S17", "S14", "S28D", "P49ST.A", "P76ST", 
                                     "P1ST", "P3ST.A", "P3ST.B", "P4ST", "P15N", "P19ST", "P28NE", 
                                     "P65ST", "P6ST", "P12ST", "P13ST")
  base.final.training = rbind(base.final.training.lv1, base.final.training.lv2)
  
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence.multiple = rbind(info.confidence.multiple, c(Faltantes[j], Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  
  for(i in 2:length(Confidence)){
    P10ST.J48 = NULL
    P10ST.J48.e = NULL
    P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
    P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
    info.confidence.multiple = rbind(info.confidence.multiple, c(Faltantes[j], Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"]))
  }
  print(j)
  print(table(base.final.training[,1]))
  print(table(base.final.training[,3]))
}
proc.time() - ptm
beep(8)

info.confidence.unico = as.data.frame(info.confidence.unico)
info.confidence.multiple = as.data.frame(info.confidence.multiple)



info.confidence.unico.melt = melt(info.confidence.unico[,c(1,2,5,6)], id.vars = c("Confidence", "Faltantes"))
colnames(info.confidence.unico.melt) = c("Confianza", "Faltantes", "Característica", "Porcentaje")
info.confidence.unico.melt[,"Confianza"] = as.factor(info.confidence.unico.melt[,"Confianza"])
Conf.VS.Faltantes = ggplot(data = info.confidence.unico.melt, aes(x = Faltantes, y = Porcentaje, 
                                                                  group = interaction(Confianza, Característica), linetype = Característica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes

info.confidence.multiple.melt = melt(info.confidence.multiple[,c(1,2,5,6)], id.vars = c("Confidence", "Faltantes"))
colnames(info.confidence.multiple.melt) = c("Confianza", "Faltantes", "Característica", "Porcentaje")
info.confidence.multiple.melt[,"Confianza"] = as.factor(info.confidence.multiple.melt[,"Confianza"])

Conf.VS.Faltantes = ggplot(data = info.confidence.multiple.melt, aes(x = Faltantes, y = Porcentaje, 
                                                                  group = interaction(Confianza, Característica), linetype = Característica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes



info.confidence.unico.melt1 = subset(info.confidence.unico.melt, Característica == "Test")
info.confidence.multiple.melt1 = subset(info.confidence.multiple.melt, Característica == "Test")

Conf.VS.Faltantes = ggplot(data = info.confidence.unico.melt1, aes(x = Faltantes, y = Porcentaje, 
                                                                   group = interaction(Confianza, Característica), linetype = Característica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes

info.confidence.unico.melt1 = subset(info.confidence.unico.melt, Característica == "Entrenamiento")

Conf.VS.Faltantes = ggplot(data = info.confidence.unico.melt1, aes(x = Faltantes, y = Porcentaje, 
                                                                   group = interaction(Confianza, Característica), linetype = Característica, colour = Confianza)) + geom_line(size = 1.2)
Conf.VS.Faltantes
