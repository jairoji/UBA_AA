library(RWeka)
library(ggplot2)
library(reshape)
library(xlsx)

setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/UBA_AA/TP1/Datasets/")

base.final = read.arff("Base_Final.arff")

set.seed(12345)

muestra = sample(1:nrow(base.final), size = nrow(base.final)*.8, replace = F)

base.final = base.final[,-which(names(base.final)=="IDENPA")]

base.final.training = base.final[muestra,]
base.final.test = base.final[-muestra,]

#write.csv(base.final.training, "Base_final_LB_Training.csv", row.names = F)
#write.csv(base.final.test, "Base_final_LB_Test.csv", row.names = F)
Confidence = seq(0.025, 0.5, by = 0.025)
P10ST.J48 = J48(P10ST ~ . , data=base.final.training, control = Weka_control(C = Confidence[1], M = 2))
P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
info.confidence = matrix(ncol = 5, nrow = length(Confidence))
colnames(info.confidence) = c("Confidence", "Tamaño", "Hojas", "Entrenamiento", "Test")
info.confidence[1,] = c(Confidence[1], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])

ptm <- proc.time()
for(i in 2:length(Confidence)){
  P10ST.J48 = J48(P10ST~., data=base.final.training, control = Weka_control(C = Confidence[i], M = 2))
  P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = base.final.test)
  info.confidence[i,] = c(Confidence[i], P10ST.J48$classifier$measureTreeSize(), P10ST.J48$classifier$measureNumLeaves(), summary(P10ST.J48)$details["pctCorrect"], P10ST.J48.e$details["pctCorrect"])
}
proc.time() - ptm
beep(8)

setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/UBA_AA/TP1/Resultados/")

info.confidence = as.data.frame(info.confidence)
info.confidence1 = melt(info.confidence[,c(1,4,5)], id.vars = "Confidence")
colnames(info.confidence1) = c("Confianza", "Medida", "Porcentaje")
Conf.vs.Ajuste = ggplot(data = info.confidence1, aes(x = Confianza, y = Porcentaje, group = Medida, colour = Medida)) + geom_line(size = 2)
#Conf.vs.Ajuste = Conf.vs.Ajuste + geom_line(data = info.confidence, aes(x = Confidence, y = Test.Percentage), col = 2)
#Conf.vs.Ajuste + xlab("Confianza") + ylab("Ajuste") + ggtitle("Gr?fico de confianza / Ajuste")
Conf.vs.Ajuste = Conf.vs.Ajuste + ggtitle("Gráfico de confianza vs performance") + xlab("Confianza") + ylab("Performance")
Conf.vs.Ajuste = Conf.vs.Ajuste + theme(plot.title = element_text(size = 20))
Conf.vs.Ajuste = Conf.vs.Ajuste + theme(axis.text = element_text(size = 15))
Conf.vs.Ajuste = Conf.vs.Ajuste + theme(axis.title.x = element_text(size = 15))
Conf.vs.Ajuste = Conf.vs.Ajuste + theme(axis.title.y = element_text(size = 13))
Conf.vs.Ajuste = Conf.vs.Ajuste + theme(legend.text = element_text(size=13))
Conf.vs.Ajuste = Conf.vs.Ajuste + theme(legend.title = element_text(size=15))
Conf.vs.Ajuste

?par
png("1.1Confianza.Performance.png", width = 800, height = 600)
Conf.vs.Ajuste
#ggplot(x=info.confidence[,"Confidence"], y=info.confidence[,"Training.Percentage"], main = "Confianza Vs Ajuste", type = "l", xlab = "Confianza", ylab = "Ajuste", ylim = c(min(info.confidence[,"Test.Percentage"])-1,  max(info.confidence[,"Training.Percentage"])+1))
#lines(x=info.confidence[,"Confidence"], y=info.confidence[,"Test.Percentage"], type = "l", col = "red")
#legend("topleft", legend = c("Entrenamiento", "Prueba"), col = c(1,2), lty = c(1,1))
dev.off()


info.confidence2 = melt(info.confidence[,c(1,2,3)], id.vars = "Confidence")
colnames(info.confidence2) = c("Confianza", "Característica", "Número")
Conf.vs.Tamano = ggplot(data = info.confidence2, aes(x = Confianza, y = Número, group = Característica, colour = Característica)) + geom_line(size = 2)
Conf.vs.Tamano = Conf.vs.Tamano + ggtitle("Gráfico de confianza vs tamaño del árbol y número de hojas") + xlab("Confianza") + ylab("Tamaño y número")
Conf.vs.Tamano = Conf.vs.Tamano + theme(plot.title = element_text(size = 20))
Conf.vs.Tamano = Conf.vs.Tamano + theme(axis.text = element_text(size = 15))
Conf.vs.Tamano = Conf.vs.Tamano + theme(axis.title.x = element_text(size = 15))
Conf.vs.Tamano = Conf.vs.Tamano + theme(axis.title.y = element_text(size = 13))
Conf.vs.Tamano = Conf.vs.Tamano + theme(legend.text = element_text(size=13))
Conf.vs.Tamano = Conf.vs.Tamano + theme(legend.title = element_text(size=15))
Conf.vs.Tamano


png("1.2.Confianza.Num.png", width = 800, height = 600)
Conf.vs.Tamano
dev.off()

png("1.3.Conf.Perf.Num.png", width = 1000, height = 400)
multiplot(Conf.vs.Ajuste, Conf.vs.Tamano, cols = 2)
##La función multiplot está en http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
dev.off()

write.xlsx(info.confidence, "1.Confianza.xlsx", sheetName = "Confianza")
#write.arff(base.final.training, "Base_final_LB_Training.arff")
#write.arff(base.final.test, "Base_final_LB_Test.arff")
