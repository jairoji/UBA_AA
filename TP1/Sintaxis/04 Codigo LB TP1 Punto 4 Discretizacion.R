#
# Aprendizaje Automatico
# TP 1
# Punto 4 
#
packs.4 <-c("foreign", "car", "dplyr", "reshape", "RWeka", "party", "partykit","ggplot2")
lapply(packs.4, library, character.only=TRUE)

setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/UBA_AA/TP1/Datasets")
#setwd("/home/sergio/Documents/Research/Data Mining FCEN/clases/AA/TP1/UBA_AA/TP1/Datasets")  

#/////////////////////////////////////////
#' 
#' discret(dataset, bins, estrategia)
#'
#' Función para producir datasets con variables numéricas discretizadas
#' 
#' Parámetros requeridos:
#'
#'  @param dataset: input (data.frame) que contiene variables numéricas
#'  @param bins:    cantidad (entero) de bins para la creación de intervalos
#'  @param estrategia:  una de tres estrategias posibles de discretización: 
#'                   a) "densidad", b) "frecuencia", ó c) "supervisado"
#'  @return result: output (data.frame), dataset con variables numéricas discretizadas
#'  
discret <- function(dataset, bins, estrategia) {
  require("RWeka")
  # Crea filtro de discretización para métodos no supervisados
  if (exists("DiscretFiltUnsup") == FALSE){
    DiscretFiltUnsup <- make_Weka_filter("weka/filters/unsupervised/attribute/Discretize")
  }
  if (estrategia == "densidad") {
    dataset.d <- DiscretFiltUnsup( ~ ., data=dataset, 
                      control = Weka_control(Y=TRUE, F=FALSE, B=bins))
  } else if (estrategia == "frecuencia") {
    dataset.d <- DiscretFiltUnsup( ~ ., data=dataset, 
                      control = Weka_control(Y=FALSE, F=TRUE, B=bins))
  } else if (estrategia == "supervisado") {
    dataset.d <- Discretize(P10ST ~ ., data=dataset)
  }
  return(dataset.d)
}
#/////////////////////////////////////////

# Leer base y particionar sets entrenamiento y test
base.final = read.arff("Base_Final.arff")
base.final = base.final[,-which(names(base.final)=="IDENPA")]

# Crea filtro de discretización para métodos no supervisados
DiscretFiltUnsup <- make_Weka_filter("weka/filters/unsupervised/attribute/Discretize")

# Vectores de confianza y número de bins según consigna
Confidence = seq(0.05, 0.5, by = 0.05)
Bins = seq(1, 20, by = 1)
Estrategias = c("densidad", "frecuencia", "supervisado")

# Matriz para carga de resultados
info.confidence4 = as.data.frame(matrix(ncol = 7))
colnames(info.confidence4) = c("Estrategia","Bins","Confidence", "Tamaño",
                               "Hojas", "Entrenamiento", "Test")

# Main loop
fila = 0 
system.time(
  for (j in Estrategias){
    for (i in 1:length(Bins)){
      dataset.d <- discret(dataset = base.final, bins = i, estrategia = j)
      # partición de base de entrenamiento y test
      set.seed(12345)
      muestra = sample(1:nrow(dataset.d), size = nrow(base.final)*.8, replace = F)
      training.d = as.data.frame(dataset.d[muestra,])
      test.d = as.data.frame(dataset.d[-muestra,])
      # iteración de bins y estrategias para rango de Confidence Punto 1
      for (c in 1:length(Confidence)){
        P10ST.J48 = J48(P10ST~., data=training.d, 
                        control = Weka_control(C = Confidence[c], M = 10))
        P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = test.d)
        # if (c==length(Confidence)){fila <- fila - 1} else {fila <- fila + i}
        fila <- fila + 1
        info.confidence4[fila, 2:7] = data.frame(Bins[i], Confidence[c],
                                 P10ST.J48$classifier$measureTreeSize(),
                                 P10ST.J48$classifier$measureNumLeaves(),
                                 summary(P10ST.J48)$details["pctCorrect"], 
                                 P10ST.J48.e$details["pctCorrect"])
        info.confidence4[fila, 1] = j
        # print(paste("c=", c, "fila=", fila))
      } # Confianza
      # print(paste("i=", i, "fila=", fila))
    } # Bins
    # print(paste("j=", j, "fila=", fila))
  } # Estrategias
) # time
info.confidence4[,1] = as.factor(info.confidence4[,1])
levels(info.confidence4[,1]) = Estrategias

# Gráficos
# plot(info.confidence4$Bins, info.confidence4$Tamaño)
# head(info.confidence4$Bins)
info.confidence4 = as.data.frame(info.confidence4)
# c("Estrategia","Bins","Confidence", "Tamaño",
#   "Hojas", "Entrenamiento", "Test")
info.conf4.dens = subset(info.confidence4, Estrategia=="densidad")
info.conf4.frec = subset(info.confidence4, Estrategia=="frecuencia")
info.conf4.superv = subset(info.confidence4, Estrategia=="supervisado")

# grafico discret densidad tamano
info.conf4.melt.dens.tam = melt(info.conf4.dens[,c(2,3,4,5)], 
                                id.vars = c("Confidence", "Bins"))
colnames(info.conf4.melt.dens.tam) = c("Confianza", "Bins", "Característica", "Número")
info.conf4.melt.dens.tam$Confianza = as.factor(info.conf4.melt.dens.tam$Confianza)

info.conf4.melt.dens.tam.g = ggplot(data = info.conf4.melt.dens.tam, 
                                    aes(x= Bins, y=Número,
                                        group = interaction(Confianza, Característica), 
                                        colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
info.conf4.melt.dens.tam.g

# grafico discret densidad performance
info.conf4.melt.dens.perf = melt(info.conf4.dens[,c(2,3,6,7)], 
                                id.vars = c("Confidence", "Bins"))
colnames(info.conf4.melt.dens.perf) = c("Confianza", "Bins", "Medida", "Performance")
info.conf4.melt.dens.perf$Confianza = as.factor(info.conf4.melt.dens.perf$Confianza)

info.conf4.melt.dens.perf.g = ggplot(data = info.conf4.melt.dens.perf, 
                                    aes(x= Bins, y=Performance,
                                        group = interaction(Confianza, Medida), 
                                        colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
info.conf4.melt.dens.perf.g #+ ylim(0,100)

# grafico discret frecuencia tamano
info.conf4.melt.frec.tam = melt(info.conf4.frec[,c(2,3,4,5)], 
                                id.vars = c("Confidence", "Bins"))
colnames(info.conf4.melt.frec.tam) = c("Confianza", "Bins", "Característica", "Número")
info.conf4.melt.frec.tam$Confianza = as.factor(info.conf4.melt.frec.tam$Confianza)

info.conf4.melt.frec.tam.g = ggplot(data = info.conf4.melt.frec.tam, 
                                    aes(x= Bins, y=Número,
                                        group = interaction(Confianza, Característica), 
                                        colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
info.conf4.melt.frec.tam.g

# grafico discret frecuencia performance
info.conf4.melt.frec.perf = melt(info.conf4.frec[,c(2,3,6,7)], 
                                 id.vars = c("Confidence", "Bins"))
colnames(info.conf4.melt.frec.perf) = c("Confianza", "Bins", "Medida", "Performance")
info.conf4.melt.frec.perf$Confianza = as.factor(info.conf4.melt.frec.perf$Confianza)

info.conf4.melt.frec.perf.g = ggplot(data = info.conf4.melt.frec.perf, 
                                     aes(x= Bins, y=Performance,
                                         group = interaction(Confianza, Medida), 
                                         colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
info.conf4.melt.frec.perf.g #+ ylim(0,100)

# grafico discret supervisado tamano
info.conf4.melt.superv.tam = melt(info.conf4.superv[,c(2,3,4,5)], 
                                id.vars = c("Confidence", "Bins"))
colnames(info.conf4.melt.superv.tam) = c("Confianza", "Bins", "Característica", "Número")
info.conf4.melt.superv.tam$Confianza = as.factor(info.conf4.melt.superv.tam$Confianza)

info.conf4.melt.superv.tam.g = ggplot(data = info.conf4.melt.superv.tam, 
                                    aes(x= Bins, y=Número,
                                        group = interaction(Confianza, Característica), 
                                        colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
info.conf4.melt.superv.tam.g

# grafico discret supervisado performance
info.conf4.melt.superv.perf = melt(info.conf4.superv[,c(2,3,6,7)], 
                                 id.vars = c("Confidence", "Bins"))
colnames(info.conf4.melt.superv.perf) = c("Confianza", "Bins", "Medida", "Performance")
info.conf4.melt.superv.perf$Confianza = as.factor(info.conf4.melt.superv.perf$Confianza)

info.conf4.melt.superv.perf.g = ggplot(data = info.conf4.melt.superv.perf, 
                                     aes(x= Bins, y=Performance,
                                         group = interaction(Confianza, Medida), 
                                         colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
info.conf4.melt.superv.perf.g #+ ylim(0,100)


# #----------------- 
# # grafico discret supervisado performance
# control.edad = melt(base.final[,c("S17", "P10ST")])
# names(control.edad)
# head(control.edad)
# colnames(control.edad) = c("P10ST", "Variable", "Edad")
# 
# control.edad.g = ggplot(data = control.edad, aes(x=P10ST , y=Edad)) + geom_boxplot()
# control.edad.g
# 

#-====================== EDAD =========================
# Leer base y particionar sets entrenamiento y test
base.edad = base.final[, which(
  names(base.final) %in% c("P10ST","S17","S15"))]

# Matriz para carga de resultados
info.confi.edad = as.data.frame(matrix(ncol = 7))
colnames(info.confi.edad) = c("Estrategia","Bins","Confidence", "Tamaño",
                               "Hojas", "Entrenamiento", "Test")

# Main loop
fila = 0 
system.time(
  for (j in Estrategias){
    for (i in 1:length(Bins)){
      dataset.d <- discret(dataset = base.edad, bins = i, estrategia = j)
      # partición de base de entrenamiento y test
      set.seed(12345)
      muestra = sample(1:nrow(dataset.d), size = nrow(base.edad)*.8, replace = F)
      training.d = as.data.frame(dataset.d[muestra,])
      test.d = as.data.frame(dataset.d[-muestra,])
      # iteración de bins y estrategias para rango de Confidence Punto 1
      for (c in 1:length(Confidence)){
        P10ST.J48 = J48(P10ST~., data=training.d, 
                        control = Weka_control(C = Confidence[c], M = 10))
        P10ST.J48.e <- evaluate_Weka_classifier(P10ST.J48, newdata = test.d)
        # if (c==length(Confidence)){fila <- fila - 1} else {fila <- fila + i}
        fila <- fila + 1
        info.confi.edad[fila, 2:7] = data.frame(Bins[i], Confidence[c],
                                                 P10ST.J48$classifier$measureTreeSize(),
                                                 P10ST.J48$classifier$measureNumLeaves(),
                                                 summary(P10ST.J48)$details["pctCorrect"], 
                                                 P10ST.J48.e$details["pctCorrect"])
        info.confi.edad[fila, 1] = j
        print(paste("c=", c, "fila=", fila))
      } # Confianza
      print(paste("i=", i, "fila=", fila))
    } # Bins
    print(paste("j=", j, "fila=", fila))
  } # Estrategias
) # time
info.confi.edad[,1] = as.factor(info.confi.edad[,1])
levels(info.confi.edad[,1]) = Estrategias

#---------------------------
# Gráficos
#---------------------------

setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/UBA_AA/TP1/Resultados")

info.confi.edad = as.data.frame(info.confi.edad)
info.cfedad.dens = subset(info.confi.edad, Estrategia=="densidad")
info.cfedad.frec = subset(info.confi.edad, Estrategia=="frecuencia")
info.cfedad.superv = subset(info.confi.edad, Estrategia=="supervisado")

# grafico discret densidad tamano
info.cfedad.melt.dens.tam = melt(info.cfedad.dens[,c(2,3,4,5)], 
                                id.vars = c("Confidence", "Bins"))
colnames(info.cfedad.melt.dens.tam) = c("Confianza", "Bins", "Característica", "Número")
info.cfedad.melt.dens.tam$Confianza = as.factor(info.cfedad.melt.dens.tam$Confianza)

info.cfedad.melt.dens.tam.g = ggplot(data = info.cfedad.melt.dens.tam, 
                                    aes(x= Bins, y=Número,
                                        group = interaction(Confianza, Característica), 
                                        colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
info.cfedad.melt.dens.tam.g = info.cfedad.melt.dens.tam.g + ggtitle("Gráfico de bins vs tamaño - Densidad") + xlab("Bins") + ylab("Número")
info.cfedad.melt.dens.tam.g = info.cfedad.melt.dens.tam.g + theme(plot.title = element_text(size = 20))
info.cfedad.melt.dens.tam.g = info.cfedad.melt.dens.tam.g + theme(axis.text = element_text(size = 15))
info.cfedad.melt.dens.tam.g = info.cfedad.melt.dens.tam.g + theme(axis.title.x = element_text(size = 15))
info.cfedad.melt.dens.tam.g = info.cfedad.melt.dens.tam.g + theme(axis.title.y = element_text(size = 13))
info.cfedad.melt.dens.tam.g = info.cfedad.melt.dens.tam.g + theme(legend.text = element_text(size=13))
info.cfedad.melt.dens.tam.g = info.cfedad.melt.dens.tam.g + theme(legend.title = element_text(size=15))

info.cfedad.melt.dens.tam.g

# grafico discret densidad performance
info.cfedad.melt.dens.perf = melt(info.cfedad.dens[,c(2,3,6,7)], 
                                 id.vars = c("Confidence", "Bins"))
colnames(info.cfedad.melt.dens.perf) = c("Confianza", "Bins", "Medida", "Performance")
info.cfedad.melt.dens.perf$Confianza = as.factor(info.cfedad.melt.dens.perf$Confianza)

info.cfedad.melt.dens.perf.g = ggplot(data = info.cfedad.melt.dens.perf, 
                                     aes(x= Bins, y=Performance,
                                         group = interaction(Confianza, Medida), 
                                         colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
info.cfedad.melt.dens.perf.g = info.cfedad.melt.dens.perf.g + ggtitle("Gráfico de bins vs performance - Densidad") + xlab("Bins") + ylab("Performance")
info.cfedad.melt.dens.perf.g = info.cfedad.melt.dens.perf.g + theme(plot.title = element_text(size = 20))
info.cfedad.melt.dens.perf.g = info.cfedad.melt.dens.perf.g + theme(axis.text = element_text(size = 15))
info.cfedad.melt.dens.perf.g = info.cfedad.melt.dens.perf.g + theme(axis.title.x = element_text(size = 15))
info.cfedad.melt.dens.perf.g = info.cfedad.melt.dens.perf.g + theme(axis.title.y = element_text(size = 13))
info.cfedad.melt.dens.perf.g = info.cfedad.melt.dens.perf.g + theme(legend.text = element_text(size=13))
info.cfedad.melt.dens.perf.g = info.cfedad.melt.dens.perf.g + theme(legend.title = element_text(size=15))

info.cfedad.melt.dens.perf.g #+ ylim(0,100)

# grafico discret frecuencia tamano
info.cfedad.melt.frec.tam = melt(info.cfedad.frec[,c(2,3,4,5)], 
                                id.vars = c("Confidence", "Bins"))
colnames(info.cfedad.melt.frec.tam) = c("Confianza", "Bins", "Característica", "Número")
info.cfedad.melt.frec.tam$Confianza = as.factor(info.cfedad.melt.frec.tam$Confianza)

info.cfedad.melt.frec.tam.g = ggplot(data = info.cfedad.melt.frec.tam, 
                                    aes(x= Bins, y=Número,
                                        group = interaction(Confianza, Característica), 
                                        colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
info.cfedad.melt.frec.tam.g = info.cfedad.melt.frec.tam.g + ggtitle("Gráfico de bins vs tamaño - Frecuencias") + xlab("Bins") + ylab("Número")
info.cfedad.melt.frec.tam.g = info.cfedad.melt.frec.tam.g + theme(plot.title = element_text(size = 20))
info.cfedad.melt.frec.tam.g = info.cfedad.melt.frec.tam.g + theme(axis.text = element_text(size = 15))
info.cfedad.melt.frec.tam.g = info.cfedad.melt.frec.tam.g + theme(axis.title.x = element_text(size = 15))
info.cfedad.melt.frec.tam.g = info.cfedad.melt.frec.tam.g + theme(axis.title.y = element_text(size = 13))
info.cfedad.melt.frec.tam.g = info.cfedad.melt.frec.tam.g + theme(legend.text = element_text(size=13))
info.cfedad.melt.frec.tam.g = info.cfedad.melt.frec.tam.g + theme(legend.title = element_text(size=15))
info.cfedad.melt.frec.tam.g

# grafico discret frecuencia performance
info.cfedad.melt.frec.perf = melt(info.cfedad.frec[,c(2,3,6,7)], 
                                 id.vars = c("Confidence", "Bins"))
colnames(info.cfedad.melt.frec.perf) = c("Confianza", "Bins", "Medida", "Performance")
info.cfedad.melt.frec.perf$Confianza = as.factor(info.cfedad.melt.frec.perf$Confianza)

info.cfedad.melt.frec.perf.g = ggplot(data = info.cfedad.melt.frec.perf, 
                                     aes(x= Bins, y=Performance,
                                         group = interaction(Confianza, Medida), 
                                         colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
info.cfedad.melt.frec.perf.g = info.cfedad.melt.frec.perf.g + ggtitle("Gráfico de bins vs performance - Frecuencia") + xlab("Bins") + ylab("Performance")
info.cfedad.melt.frec.perf.g = info.cfedad.melt.frec.perf.g + theme(plot.title = element_text(size = 20))
info.cfedad.melt.frec.perf.g = info.cfedad.melt.frec.perf.g + theme(axis.text = element_text(size = 15))
info.cfedad.melt.frec.perf.g = info.cfedad.melt.frec.perf.g + theme(axis.title.x = element_text(size = 15))
info.cfedad.melt.frec.perf.g = info.cfedad.melt.frec.perf.g + theme(axis.title.y = element_text(size = 13))
info.cfedad.melt.frec.perf.g = info.cfedad.melt.frec.perf.g + theme(legend.text = element_text(size=13))
info.cfedad.melt.frec.perf.g = info.cfedad.melt.frec.perf.g + theme(legend.title = element_text(size=15))
info.cfedad.melt.frec.perf.g #+ ylim(0,100)

# grafico discret supervisado tamano
info.cfedad.melt.superv.tam = melt(info.cfedad.superv[,c(2,3,4,5)], 
                                  id.vars = c("Confidence", "Bins"))
colnames(info.cfedad.melt.superv.tam) = c("Confianza", "Bins", "Característica", "Número")
info.cfedad.melt.superv.tam$Confianza = as.factor(info.cfedad.melt.superv.tam$Confianza)

info.cfedad.melt.superv.tam.g = ggplot(data = info.cfedad.melt.superv.tam, 
                                      aes(x= Bins, y=Número,
                                          group = interaction(Confianza, Característica), 
                                          colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
info.cfedad.melt.superv.tam.g = info.cfedad.melt.superv.tam.g + ggtitle("Gráfico de bins vs tamaño - Supervisado") + xlab("Bins") + ylab("Número")
info.cfedad.melt.superv.tam.g = info.cfedad.melt.superv.tam.g + theme(plot.title = element_text(size = 20))
info.cfedad.melt.superv.tam.g = info.cfedad.melt.superv.tam.g + theme(axis.text = element_text(size = 15))
info.cfedad.melt.superv.tam.g = info.cfedad.melt.superv.tam.g + theme(axis.title.x = element_text(size = 15))
info.cfedad.melt.superv.tam.g = info.cfedad.melt.superv.tam.g + theme(axis.title.y = element_text(size = 13))
info.cfedad.melt.superv.tam.g = info.cfedad.melt.superv.tam.g + theme(legend.text = element_text(size=13))
info.cfedad.melt.superv.tam.g = info.cfedad.melt.superv.tam.g + theme(legend.title = element_text(size=15))
info.cfedad.melt.superv.tam.g

# grafico discret supervisado performance
info.cfedad.melt.superv.perf = melt(info.cfedad.superv[,c(2,3,6,7)], 
                                   id.vars = c("Confidence", "Bins"))
colnames(info.cfedad.melt.superv.perf) = c("Confianza", "Bins", "Medida", "Performance")
info.cfedad.melt.superv.perf$Confianza = as.factor(info.cfedad.melt.superv.perf$Confianza)

info.cfedad.melt.superv.perf.g = ggplot(data = info.cfedad.melt.superv.perf, 
                                       aes(x= Bins, y=Performance,
                                           group = interaction(Confianza, Medida), 
                                           colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)

info.cfedad.melt.superv.perf.g = info.cfedad.melt.superv.perf.g + ggtitle("Gráfico de bins vs performance - Supervisado") + xlab("Bins") + ylab("Performance")
info.cfedad.melt.superv.perf.g = info.cfedad.melt.superv.perf.g + theme(plot.title = element_text(size = 20))
info.cfedad.melt.superv.perf.g = info.cfedad.melt.superv.perf.g + theme(axis.text = element_text(size = 15))
info.cfedad.melt.superv.perf.g = info.cfedad.melt.superv.perf.g + theme(axis.title.x = element_text(size = 15))
info.cfedad.melt.superv.perf.g = info.cfedad.melt.superv.perf.g + theme(axis.title.y = element_text(size = 13))
info.cfedad.melt.superv.perf.g = info.cfedad.melt.superv.perf.g + theme(legend.text = element_text(size=13))
info.cfedad.melt.superv.perf.g = info.cfedad.melt.superv.perf.g + theme(legend.title = element_text(size=15))
info.cfedad.melt.superv.perf.g #+ ylim(0,100)

png()
multiplot()
dev.off()








#===============================================




#theme(plot.title = element_text(size = 20))
#   + theme(axis.text = element_text(size = 15))
#   + theme(axis.title.x = element_text(size = 15))
#   + theme(axis.title.y = element_text(size = 13))
#   + theme(legend.text = element_text(size=13))
#   + theme(legend.title = element_text(size=15))
# 
# png("Punto 4 Confianza VS tamaño.png", width = 800, height = 600)
# Conf.vs.Tamano
# dev.off()
# 
# info.confidence2 = as.data.frame(info.confidence2)
# colnames(info.confidence2) = c("Faltantes", "Confidence", "Tamaño", "Hojas", "Entrenamiento", "Test")
# info.confidence.melt = melt(info.confidence2[,c(1,2,5,6)], id.vars = c("Confidence", "Faltantes"))
# colnames(info.confidence.melt) = c("Confianza", "Faltantes", "Característica", "Porcentaje")
# info.confidence.melt[,"Confianza"] = as.factor(info.confidence.melt[,"Confianza"])
# Conf.VS.Faltantes = ggplot(data = info.confidence.melt, aes(x = Faltantes, y = Porcentaje, 
#                                                             group = interaction(Confianza, Característica), linetype = Característica, colour = Confianza)) + geom_line(size = 1.2)
# Conf.VS.Faltantes
# 
# info.confidence.melt.Entrenamiento = subset(info.confidence.melt, Característica == "Entrenamiento")
# info.confidence.melt.Entrenamiento[,"Característica"] = droplevels(info.confidence.melt.Entrenamiento[,"Característica"])
# 
# Conf.VS.Faltantes.Entre = ggplot(data = info.confidence.melt.Entrenamiento, aes(x = Faltantes, y = Porcentaje, 
#                                                                                 group = Confianza, colour = Confianza)) + geom_line(size = 1.2)
# Conf.VS.Faltantes.Entre
# 
# info.confidence.melt.Test = subset(info.confidence.melt, Característica == "Test")
# info.confidence.melt.Test[,"Característica"] = droplevels(info.confidence.melt.Test[,"Característica"])
# 
# Conf.VS.Faltantes.Test = ggplot(data = info.confidence.melt.Test, aes(x = Faltantes, y = Porcentaje, 
#                                                                       group = Confianza, colour = Confianza)) + geom_line(size = 1.2)
# Conf.VS.Faltantes.Test