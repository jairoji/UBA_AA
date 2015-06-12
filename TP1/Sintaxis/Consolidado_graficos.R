setwd("/run/media/ahriman/Stuff/MDMKD/Primer cuatrimestre/UBA_AA/TP1/Resultados")

library(ggplot2)
library(xlsx)
library(reshape)

Confianza = read.xlsx("1.Confianza.xlsx", sheetIndex = 1)
Confianza[["NA."]] = NULL
Faltantes.Unic.Var.Global = read.xlsx("2.1.1Col.Unic.Fil.Unic.Global.xlsx", sheetIndex = 1)
Faltantes.Unic.Var.Global[["NA."]] = NULL
colnames(Faltantes.Unic.Var.Global)[3] = "Tamaño"
Faltantes.Unic.Var.Clase = read.xlsx("2.1.2Col.Unic.Fil.Unic.Clase.xlsx", sheetIndex = 1)
Faltantes.Unic.Var.Clase[["NA."]] = NULL
colnames(Faltantes.Unic.Var.Clase)[3] = "Tamaño"
Faltantes.Mult.Col.Unic.Fil.Clase = read.xlsx("2.2.1.Mult.Col.Unic.Fil.Clase.xlsx", sheetIndex = 1)
Faltantes.Mult.Col.Unic.Fil.Clase[["NA."]] = NULL
colnames(Faltantes.Mult.Col.Unic.Fil.Clase)[3] = "Tamaño"
Faltantes.Mult.Col.Unic.Fil.Global = read.xlsx("2.2.1.Mult.Col.Unic.Fil.Global.xlsx", sheetIndex = 1)
Faltantes.Mult.Col.Unic.Fil.Global[["NA."]] = NULL
colnames(Faltantes.Mult.Col.Unic.Fil.Global)[3]
Faltantes.Mult.Col.Mult.Fil.Clase = read.xlsx("2.3.1.Mult.Col.Mult.Fil.Clase.xlsx", sheetIndex = 1)
Faltantes.Mult.Col.Mult.Fil.Clase[["NA."]] = NULL
colnames(Faltantes.Mult.Col.Mult.Fil.Clase)[3]
Faltantes.Mult.Col.Mult.Fil.Global = read.xlsx("2.3.1.Mult.Col.Mult.Fil.Global.xlsx", sheetIndex = 1)
Faltantes.Mult.Col.Mult.Fil.Global[["NA."]] = NULL
colnames(Faltantes.Mult.Col.Mult.Fil.Global)
Ruido = read.xlsx("3.Ruido.xlsx", sheetIndex = 1)
colnames(Ruido)
Ruido[["NA."]] = NULL

#######################################FAltantes


################
##Única variable
################

###############
##Global
###############

##Performance
Faltantes.Unic.Var.Global.Perf = melt(Faltantes.Unic.Var.Global[,c(1, 2, 5, 6)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Unic.Var.Global.Perf) = c("Confianza", "Faltantes", "Medida", "Performance")
Faltantes.Unic.Var.Global.Perf[,"Confianza"] = as.factor(Faltantes.Unic.Var.Global.Perf[,"Confianza"])

Faltantes.Unic.Var.Global.Perf.plot = ggplot(data = Faltantes.Unic.Var.Global.Perf, aes(x = Faltantes, y = Performance,
                                                     group = interaction(Confianza, Medida), 
                                                     colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
Faltantes.Unic.Var.Global.Perf.plot = Faltantes.Unic.Var.Global.Perf.plot + ggtitle("Gráfico de faltantes vs performance - Global") + xlab("Faltantes") + ylab("Performance")
Faltantes.Unic.Var.Global.Perf.plot = Faltantes.Unic.Var.Global.Perf.plot + theme(plot.title = element_text(size = 20))
Faltantes.Unic.Var.Global.Perf.plot = Faltantes.Unic.Var.Global.Perf.plot + theme(axis.text = element_text(size = 15))
Faltantes.Unic.Var.Global.Perf.plot = Faltantes.Unic.Var.Global.Perf.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Unic.Var.Global.Perf.plot = Faltantes.Unic.Var.Global.Perf.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Unic.Var.Global.Perf.plot = Faltantes.Unic.Var.Global.Perf.plot + theme(legend.text = element_text(size=13))
Faltantes.Unic.Var.Global.Perf.plot = Faltantes.Unic.Var.Global.Perf.plot + theme(legend.title = element_text(size=15))

Faltantes.Unic.Var.Global.Perf.plot

##Tamaño
Faltantes.Unic.Var.Global.Tamano = melt(Faltantes.Unic.Var.Global[,c(1, 2, 3, 4)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Unic.Var.Global.Tamano) = c("Confianza", "Faltantes", "Característica", "Número")
Faltantes.Unic.Var.Global.Tamano[,"Confianza"] = as.factor(Faltantes.Unic.Var.Global.Tamano[,"Confianza"])

Faltantes.Unic.Var.Global.Tamano.plot = ggplot(data = Faltantes.Unic.Var.Global.Tamano, aes(x = Faltantes, y = Número,
                                                                                        group = interaction(Confianza, Característica), 
                                                                                        colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
Faltantes.Unic.Var.Global.Tamano.plot = Faltantes.Unic.Var.Global.Tamano.plot + ggtitle("Gráfico de faltantes vs Tamaño del árbol y número de hojas - Global") + xlab("Faltantes") + ylab("Número")
Faltantes.Unic.Var.Global.Tamano.plot = Faltantes.Unic.Var.Global.Tamano.plot + theme(plot.title = element_text(size = 20))
Faltantes.Unic.Var.Global.Tamano.plot = Faltantes.Unic.Var.Global.Tamano.plot + theme(axis.text = element_text(size = 15))
Faltantes.Unic.Var.Global.Tamano.plot = Faltantes.Unic.Var.Global.Tamano.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Unic.Var.Global.Tamano.plot = Faltantes.Unic.Var.Global.Tamano.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Unic.Var.Global.Tamano.plot = Faltantes.Unic.Var.Global.Tamano.plot + theme(legend.text = element_text(size=13))
Faltantes.Unic.Var.Global.Tamano.plot = Faltantes.Unic.Var.Global.Tamano.plot + theme(legend.title = element_text(size=15))

Faltantes.Unic.Var.Global.Tamano.plot

############
##Clase
############

##Performance
Faltantes.Unic.Var.Clase.Perf = melt(Faltantes.Unic.Var.Clase[,c(1, 2, 5, 6)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Unic.Var.Clase.Perf) = c("Confianza", "Faltantes", "Medida", "Performance")
Faltantes.Unic.Var.Clase.Perf[,"Confianza"] = as.factor(Faltantes.Unic.Var.Clase.Perf[,"Confianza"])

Faltantes.Unic.Var.Clase.Perf.plot = ggplot(data = Faltantes.Unic.Var.Clase.Perf, aes(x = Faltantes, y = Performance,
                                                                                      group = interaction(Confianza, Medida), 
                                                                                      colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
Faltantes.Unic.Var.Clase.Perf.plot = Faltantes.Unic.Var.Clase.Perf.plot + ggtitle("Gráfico de faltantes vs performance - Clase") + xlab("Faltantes") + ylab("Performance")
Faltantes.Unic.Var.Clase.Perf.plot = Faltantes.Unic.Var.Clase.Perf.plot + theme(plot.title = element_text(size = 20))
Faltantes.Unic.Var.Clase.Perf.plot = Faltantes.Unic.Var.Clase.Perf.plot + theme(axis.text = element_text(size = 15))
Faltantes.Unic.Var.Clase.Perf.plot = Faltantes.Unic.Var.Clase.Perf.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Unic.Var.Clase.Perf.plot = Faltantes.Unic.Var.Clase.Perf.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Unic.Var.Clase.Perf.plot = Faltantes.Unic.Var.Clase.Perf.plot + theme(legend.text = element_text(size=13))
Faltantes.Unic.Var.Clase.Perf.plot = Faltantes.Unic.Var.Clase.Perf.plot + theme(legend.title = element_text(size=15))

Faltantes.Unic.Var.Clase.Perf.plot

##Tamaño
Faltantes.Unic.Var.Clase.Tamano = melt(Faltantes.Unic.Var.Clase[,c(1, 2, 3, 4)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Unic.Var.Clase.Tamano) = c("Confianza", "Faltantes", "Característica", "Número")
Faltantes.Unic.Var.Clase.Tamano[,"Confianza"] = as.factor(Faltantes.Unic.Var.Clase.Tamano[,"Confianza"])

Faltantes.Unic.Var.Clase.Tamano.plot = ggplot(data = Faltantes.Unic.Var.Clase.Tamano, aes(x = Faltantes, y = Número,
                                                                                          group = interaction(Confianza, Característica), 
                                                                                          colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
Faltantes.Unic.Var.Clase.Tamano.plot = Faltantes.Unic.Var.Clase.Tamano.plot + ggtitle("Gráfico de faltantes vs Tamaño del árbol y número de hojas - Clase") + xlab("Faltantes") + ylab("Número")
Faltantes.Unic.Var.Clase.Tamano.plot = Faltantes.Unic.Var.Clase.Tamano.plot + theme(plot.title = element_text(size = 20))
Faltantes.Unic.Var.Clase.Tamano.plot = Faltantes.Unic.Var.Clase.Tamano.plot + theme(axis.text = element_text(size = 15))
Faltantes.Unic.Var.Clase.Tamano.plot = Faltantes.Unic.Var.Clase.Tamano.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Unic.Var.Clase.Tamano.plot = Faltantes.Unic.Var.Clase.Tamano.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Unic.Var.Clase.Tamano.plot = Faltantes.Unic.Var.Clase.Tamano.plot + theme(legend.text = element_text(size=13))
Faltantes.Unic.Var.Clase.Tamano.plot = Faltantes.Unic.Var.Clase.Tamano.plot + theme(legend.title = element_text(size=15))

Faltantes.Unic.Var.Clase.Tamano.plot



##Múltiples variables, un faltante por fila

##########
##Global
#########

Faltantes.Mult.Col.Unic.Fil.Global.Perf = melt(Faltantes.Mult.Col.Unic.Fil.Global[,c(1, 2, 5, 6)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Mult.Col.Unic.Fil.Global.Perf) = c("Confianza", "Faltantes", "Medida", "Performance")
Faltantes.Mult.Col.Unic.Fil.Global.Perf[,"Confianza"] = as.factor(Faltantes.Mult.Col.Unic.Fil.Global.Perf[,"Confianza"])

Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot = ggplot(data = Faltantes.Mult.Col.Unic.Fil.Global.Perf, aes(x = Faltantes, y = Performance,
                                                                                                          group = interaction(Confianza, Medida), 
                                                                                                          colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot + ggtitle("Gráfico de faltantes vs performance - Global") + xlab("Faltantes") + ylab("Performance")
Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot + theme(plot.title = element_text(size = 20))
Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot + theme(axis.text = element_text(size = 15))
Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot + theme(legend.text = element_text(size=13))
Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot + theme(legend.title = element_text(size=15))

Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot

##Tamaño
Faltantes.Mult.Col.Unic.Fil.Global.Tamano = melt(Faltantes.Mult.Col.Unic.Fil.Global[,c(1, 2, 3, 4)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Mult.Col.Unic.Fil.Global.Tamano) = c("Confianza", "Faltantes", "Característica", "Número")
Faltantes.Mult.Col.Unic.Fil.Global.Tamano[,"Confianza"] = as.factor(Faltantes.Mult.Col.Unic.Fil.Global.Tamano[,"Confianza"])

Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot = ggplot(data = Faltantes.Mult.Col.Unic.Fil.Global.Tamano, aes(x = Faltantes, y = Número,
                                                                                                              group = interaction(Confianza, Característica), 
                                                                                                              colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot + ggtitle("Gráfico de faltantes vs Tamaño del árbol y número de hojas - Global") + xlab("Faltantes") + ylab("Número")
Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot + theme(plot.title = element_text(size = 20))
Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot + theme(axis.text = element_text(size = 15))
Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot + theme(legend.text = element_text(size=13))
Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot + theme(legend.title = element_text(size=15))

Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot

#########
##Clase
#########
Faltantes.Mult.Col.Unic.Fil.Clase.Perf = melt(Faltantes.Mult.Col.Unic.Fil.Clase[,c(1, 2, 5, 6)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Mult.Col.Unic.Fil.Clase.Perf) = c("Confianza", "Faltantes", "Medida", "Performance")
Faltantes.Mult.Col.Unic.Fil.Clase.Perf[,"Confianza"] = as.factor(Faltantes.Mult.Col.Unic.Fil.Clase.Perf[,"Confianza"])

Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot = ggplot(data = Faltantes.Mult.Col.Unic.Fil.Clase.Perf, aes(x = Faltantes, y = Performance,
                                                                                                        group = interaction(Confianza, Medida), 
                                                                                                        colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot + ggtitle("Gráfico de faltantes vs performance - Clase") + xlab("Faltantes") + ylab("Performance")
Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot + theme(plot.title = element_text(size = 20))
Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot + theme(axis.text = element_text(size = 15))
Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot + theme(legend.text = element_text(size=13))
Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot + theme(legend.title = element_text(size=15))

Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot


##Tamaño

Faltantes.Mult.Col.Unic.Fil.Clase.Tamano = melt(Faltantes.Mult.Col.Unic.Fil.Clase[,c(1, 2, 3, 4)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Mult.Col.Unic.Fil.Clase.Tamano) = c("Confianza", "Faltantes", "Característica", "Número")
Faltantes.Mult.Col.Unic.Fil.Clase.Tamano[,"Confianza"] = as.factor(Faltantes.Mult.Col.Unic.Fil.Clase.Tamano[,"Confianza"])

Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot = ggplot(data = Faltantes.Mult.Col.Unic.Fil.Clase.Tamano, aes(x = Faltantes, y = Número,
                                                                                                            group = interaction(Confianza, Característica), 
                                                                                                            colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot + ggtitle("Gráfico de faltantes vs Tamaño del árbol y número de hojas - Clase") + xlab("Faltantes") + ylab("Número")
Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot + theme(plot.title = element_text(size = 20))
Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot + theme(axis.text = element_text(size = 15))
Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot + theme(legend.text = element_text(size=13))
Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot + theme(legend.title = element_text(size=15))

Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot



##Múltiples variables, múltiples faltantes por fila

############
##Global
###########

###Performance
Faltantes.Mult.Col.Mult.Fil.Global.Perf = melt(Faltantes.Mult.Col.Mult.Fil.Global[,c(1, 2, 5, 6)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Mult.Col.Mult.Fil.Global.Perf) = c("Confianza", "Faltantes", "Medida", "Performance")
Faltantes.Mult.Col.Mult.Fil.Global.Perf[,"Confianza"] = as.factor(Faltantes.Mult.Col.Mult.Fil.Global.Perf[,"Confianza"])

Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot = ggplot(data = Faltantes.Mult.Col.Mult.Fil.Global.Perf, aes(x = Faltantes, y = Performance,
                                                                                                        group = interaction(Confianza, Medida), 
                                                                                                        colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot + ggtitle("Gráfico de faltantes vs performance - Global") + xlab("Faltantes") + ylab("Performance")
Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot + theme(plot.title = element_text(size = 20))
Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot + theme(axis.text = element_text(size = 15))
Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot + theme(legend.text = element_text(size=13))
Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot + theme(legend.title = element_text(size=15))

Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot

##Tamaño
Faltantes.Mult.Col.Mult.Fil.Global.Tamano = melt(Faltantes.Mult.Col.Unic.Fil.Global[,c(1, 2, 3, 4)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Mult.Col.Mult.Fil.Global.Tamano) = c("Confianza", "Faltantes", "Característica", "Número")
Faltantes.Mult.Col.Mult.Fil.Global.Tamano[,"Confianza"] = as.factor(Faltantes.Mult.Col.Mult.Fil.Global.Tamano[,"Confianza"])

Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot = ggplot(data = Faltantes.Mult.Col.Mult.Fil.Global.Tamano, aes(x = Faltantes, y = Número,
                                                                                                              group = interaction(Confianza, Característica), 
                                                                                                              colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot + ggtitle("Gráfico de faltantes vs Tamaño del árbol y número de hojas - Global") + xlab("Faltantes") + ylab("Número")
Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot + theme(plot.title = element_text(size = 20))
Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot + theme(axis.text = element_text(size = 15))
Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot + theme(legend.text = element_text(size=13))
Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot + theme(legend.title = element_text(size=15))

Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot

#########
##Clase
########

##Performance

Faltantes.Mult.Col.Mult.Fil.Clase.Perf = melt(Faltantes.Mult.Col.Mult.Fil.Clase[,c(1, 2, 5, 6)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Mult.Col.Mult.Fil.Clase.Perf) = c("Confianza", "Faltantes", "Medida", "Performance")
Faltantes.Mult.Col.Mult.Fil.Clase.Perf[,"Confianza"] = as.factor(Faltantes.Mult.Col.Mult.Fil.Clase.Perf[,"Confianza"])

Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot = ggplot(data = Faltantes.Mult.Col.Mult.Fil.Clase.Perf, aes(x = Faltantes, y = Performance,
                                                                                        group = interaction(Confianza, Medida), 
                                                                                        colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot + ggtitle("Gráfico de faltantes vs performance - Clase") + xlab("Faltantes") + ylab("Performance")
Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot + theme(plot.title = element_text(size = 20))
Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot + theme(axis.text = element_text(size = 15))
Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot + theme(legend.text = element_text(size=13))
Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot + theme(legend.title = element_text(size=15))

Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot


##Tamaño

Faltantes.Mult.Col.Mult.Fil.Clase.Tamano = melt(Faltantes.Mult.Col.Unic.Fil.Clase[,c(1, 2, 3, 4)], id.vars = c("Confidence", "Faltantes"))
colnames(Faltantes.Mult.Col.Mult.Fil.Clase.Tamano) = c("Confianza", "Faltantes", "Característica", "Número")
Faltantes.Mult.Col.Mult.Fil.Clase.Tamano[,"Confianza"] = as.factor(Faltantes.Mult.Col.Mult.Fil.Clase.Tamano[,"Confianza"])

Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot = ggplot(data = Faltantes.Mult.Col.Mult.Fil.Clase.Tamano, aes(x = Faltantes, y = Número,
                                                                                                            group = interaction(Confianza, Característica), 
                                                                                                            colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot + ggtitle("Gráfico de faltantes vs Tamaño del árbol y número de hojas - Clase") + xlab("Faltantes") + ylab("Número")
Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot + theme(plot.title = element_text(size = 20))
Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot + theme(axis.text = element_text(size = 15))
Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot + theme(axis.title.x = element_text(size = 15))
Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot + theme(axis.title.y = element_text(size = 13))
Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot + theme(legend.text = element_text(size=13))
Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot = Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot + theme(legend.title = element_text(size=15))

Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot
############################################################################################
##############################################Ruido#########################################
############################################################################################

RuidoPerf = melt(Ruido[,c(1, 2, 5, 6)], id.vars = c("Confidence", "Ruido"))
colnames(RuidoPerf) = c("Confianza", "Ruido", "Medida", "Performance")
RuidoPerf[,"Confianza"] = as.factor(RuidoPerf[,"Confianza"])

RuidoPerf.plot = ggplot(data = RuidoPerf, aes(x = Ruido, y = Performance,
                                              group = interaction(Confianza, Medida), 
                                              colour = Confianza, linetype = Medida)) + geom_line(size = 1.2)
RuidoPerf.plot = RuidoPerf.plot + ggtitle("Gráfico de Ruido vs performance") + xlab("Ruido") + ylab("Performance")
RuidoPerf.plot = RuidoPerf.plot + theme(plot.title = element_text(size = 20))
RuidoPerf.plot = RuidoPerf.plot + theme(axis.text = element_text(size = 15))
RuidoPerf.plot = RuidoPerf.plot + theme(axis.title.x = element_text(size = 15))
RuidoPerf.plot = RuidoPerf.plot + theme(axis.title.y = element_text(size = 13))
RuidoPerf.plot = RuidoPerf.plot + theme(legend.text = element_text(size=13))
RuidoPerf.plot = RuidoPerf.plot + theme(legend.title = element_text(size=15))

RuidoPerf.plot


##Tamaño

RuidoTamano = melt(Ruido[,c(1, 2, 3, 4)], id.vars = c("Confidence", "Ruido"))
colnames(RuidoTamano) = c("Confianza", "Ruido", "Característica", "Número")
RuidoTamano[,"Confianza"] = as.factor(RuidoTamano[,"Confianza"])

RuidoTamano.plot = ggplot(data = RuidoTamano, aes(x = Ruido, y = Número,
                                                  group = interaction(Confianza, Característica), 
                                                  colour = Confianza, linetype = Característica)) + geom_line(size = 1.2)
RuidoTamano.plot = RuidoTamano.plot + ggtitle("Gráfico de Ruido vs Tamaño del árbol y número de hojas") + xlab("Ruido") + ylab("Número")
RuidoTamano.plot = RuidoTamano.plot + theme(plot.title = element_text(size = 20))
RuidoTamano.plot = RuidoTamano.plot + theme(axis.text = element_text(size = 15))
RuidoTamano.plot = RuidoTamano.plot + theme(axis.title.x = element_text(size = 15))
RuidoTamano.plot = RuidoTamano.plot + theme(axis.title.y = element_text(size = 13))
RuidoTamano.plot = RuidoTamano.plot + theme(legend.text = element_text(size=13))
RuidoTamano.plot = RuidoTamano.plot + theme(legend.title = element_text(size=15))

RuidoTamano.plot

###Exportando los gráficos 
png("2_1_Unic_Col.png", width = 1600, height = 1200)
multiplot(Faltantes.Unic.Var.Global.Perf.plot, Faltantes.Unic.Var.Global.Tamano.plot, 
          Faltantes.Unic.Var.Clase.Perf.plot, Faltantes.Unic.Var.Clase.Tamano.plot, cols = 2)
dev.off()

png("2_1_Mult_Col_Unic_Fil.png", width = 1600, height = 1200)
multiplot(Faltantes.Mult.Col.Unic.Fil.Global.Perf.plot, Faltantes.Mult.Col.Unic.Fil.Global.Tamano.plot, 
          Faltantes.Mult.Col.Unic.Fil.Clase.Perf.plot, Faltantes.Mult.Col.Unic.Fil.Clase.Tamano.plot, cols = 2)
dev.off()

png("2_1_Mult_Col_Mult_Fil.png", width = 1600, height = 1200)
multiplot(Faltantes.Mult.Col.Mult.Fil.Global.Perf.plot, Faltantes.Mult.Col.Mult.Fil.Global.Tamano.plot, 
          Faltantes.Mult.Col.Mult.Fil.Clase.Perf.plot, Faltantes.Mult.Col.Mult.Fil.Clase.Tamano.plot, cols = 2)
dev.off()


png("3_1_Ruido.png", width = 1000, height = 400)
multiplot(RuidoPerf.plot, RuidoTamano.plot, cols = 2)
dev.off()


