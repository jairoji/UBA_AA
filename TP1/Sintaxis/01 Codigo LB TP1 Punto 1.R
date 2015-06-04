library(RWeka)
library(ggplot2)
library(reshape)

setwd("C:/Users/Ahriman/Google Drive/UBA Aprendizaje automático/TP1/Datasets")

base.final = read.arff("Base_Final.arff")

set.seed(12345)

muestra = sample(1:nrow(base.final), size = nrow(base.final)*.8, replace = F)

base.final = base.final[,-which(names(base.final)=="IDENPA")]

base.final.training = base.final[muestra,]
base.final.test = base.final[-muestra,]

#write.csv(base.final.training, "Base_final_LB_Training.csv", row.names = F)
#write.csv(base.final.test, "Base_final_LB_Test.csv", row.names = F)

Confidence = seq(0.025, 0.5, by = 0.025)
P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[1], M = 10))
P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
info.confidence = matrix(ncol = 5, nrow = length(Confidence))
colnames(info.confidence) = c("Confidence", "Tamaño", "Hojas", "Entrenamiento", "Test")
info.confidence[1,] = c(Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])

date()
for(i in 2:length(Confidence)){
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 10))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence[i,] = c(Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])
}
date()



setwd("C:/Users/Ahriman/Google Drive/UBA Aprendizaje automático/TP1/Resultados")

info.confidence = as.data.frame(info.confidence)
info.confidence1 = melt(info.confidence[,c(1,4,5)], id.vars = "Confidence")
colnames(info.confidence1) = c("Confianza", "Set", "Porcentaje")
Conf.vs.Ajuste = ggplot(data = info.confidence1, aes(x = Confianza, y = Porcentaje, group = Set, colour = Set)) + geom_line()
#Conf.vs.Ajuste = Conf.vs.Ajuste + geom_line(data = info.confidence, aes(x = Confidence, y = Test.Percentage), col = 2)
#Conf.vs.Ajuste + xlab("Confianza") + ylab("Ajuste") + ggtitle("Gráfico de confianza / Ajuste")

png("Punto 1 Confianza VS ajuste.png", width = 800, height = 600)
Conf.vs.Ajuste
#ggplot(x=info.confidence[,"Confidence"], y=info.confidence[,"Training.Percentage"], main = "Confianza Vs Ajuste", type = "l", xlab = "Confianza", ylab = "Ajuste", ylim = c(min(info.confidence[,"Test.Percentage"])-1,  max(info.confidence[,"Training.Percentage"])+1))
#lines(x=info.confidence[,"Confidence"], y=info.confidence[,"Test.Percentage"], type = "l", col = "red")
#legend("topleft", legend = c("Entrenamiento", "Prueba"), col = c(1,2), lty = c(1,1))
dev.off()

info.confidence2 = melt(info.confidence[,c(1,2,3)], id.vars = "Confidence")
colnames(info.confidence2) = c("Confianza", "Característica", "Número")
Conf.vs.Tamano = ggplot(data = info.confidence2, aes(x = Confianza, y = Número, group = Característica, colour = Característica)) + geom_line()

png("Punto 1 Confianza VS tamaño.png", width = 800, height = 600)
Conf.vs.Tamano
dev.off()


#write.arff(base.final.training, "Base_final_LB_Training.arff")
#write.arff(base.final.test, "Base_final_LB_Test.arff")
