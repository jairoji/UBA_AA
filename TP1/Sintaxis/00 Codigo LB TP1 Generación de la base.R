setwd("C:/Users/Ahriman/Google Drive/UBA Aprendizaje automático/TP1/Datasets")}

library(foreign)
library(RWeka)
library(party)
library(partykit)
library(car)
library(dplyr)
#library(xlsx)

latino11 <- read.spss("Latinobarometro_2011_esp.sav", to.data.frame = T)
latino11 = latino11[,-c(3:35)]
latino11 = latino11[,-c(369:ncol(latino11))]  

latino11Sur = subset(latino11, IDENPA %in% c("Argentina", "Bolivia", "Brazil", "Chile", 
                                             "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay",
                                             "Venezuela"))

rm(latino11)

for(i in 1:ncol(latino11Sur)){
  if (is.factor(latino11Sur[,i])){
    latino11Sur[,i] = droplevels(latino11Sur[,i])
  }
  else
    next
}

#for(i in 1:ncol(pregunas.interes)){
#  print(names(pregunas.interes)[i])
#  print(paste("NA's", length(which(is.na(pregunas.interes[,i])==T))))
#  print(table(pregunas.interes[,i]))
#}
#plot(latino11Sur[,"P27NE.B"])
#plot(latino11Sur[,"P28NE"])
#plot(density(latino11Sur[,"P60N"], na.rm = T, adjust = 1.5), main = "Desidad de la transparencia del gobierno", xlim = c(1,100))
#table(latino11Sur[,"P5STIC1A"])
#table(latino11Sur[,"P5NICC2B"])
#names(latino11Sur)
#plot(latino11Sur[,"S1NICC7"]) ##Se podrían mezclar las categorías poco y nada en una sola "Poco-Nada" para nivelar las categorías
#plot(latino11Sur[,"S2NICC8"])
#a$p.value

#names(latino11Sur)
base.final = latino11Sur[,c("S1NICC7", "S15", "S16", "S17", "S14", "S28D", "P49ST.A", 
                            "P76ST", "IDENPA", "P1ST", "P3ST.A", "P3ST.B", "P4ST", "P10ST", 
                            "P15N", "P19ST", "P28NE", "P65ST", "P6ST", "P12ST", "P13ST")]

#base.final = latino11Sur[, c("P37ST", "S15", "S16", "S17", "S14", "S28D", "P49ST.A", 
#                           "P49ST.B", "P49ST.E", "P76ST", "P10ST", "P4ST", "S1NICC7", 
#                           "P6ST", "P12ST", "P13ST", "IDENPA")]
#PValores = as.data.frame(matrix(ncol = 2))
#PValores = rbind(PValores, c(names(base.final)[2], chis$p.value))
#for(i in 3:ncol(base.final)){
#  if(is.factor(base.final[,i])){
#    chis = chisq.test(table(base.final[,"S1NICC7"], base.final[,i]))
#    PValores = rbind(PValores, c(names(base.final)[i], chis$p.value))
#  }
#  else
#    next
#}

#PValores = PValores[-1,]
#nrow(PValores)
#PValores[order(PValores[,2], decreasing = T),]


base.final = base.final[-which(is.na(base.final[,"P10ST"])==T),]

rm(latino11Sur)
#rm(pregunas.interes)

for(i in c(1:3, 5:ncol(base.final)))
  base.final[,i] = droplevels(base.final[,i])

levels(base.final[,7])
which(is.na(base.final[,7])==T)

for(i in 1:ncol(base.final)){
  if (is.factor(base.final[,i])){
    base.final[,i] <- as.character(base.final[,i])
    base.final[which(is.na(base.final[,i])==T),i] = "NS/NR"
    base.final[,i] <- as.factor(base.final[,i])
  }
  else
    next
}



##Recodificanco la variable de autoposicionamiento entre Izquierda y Derecha
base.final[,"P76ST"] = recode(base.final[,"P76ST"], "c('00 Izquierda', '1', '2', '3') = 'Izquierda';
                              c('4', '5', '6') = 'Centro'; c('7', '8', '9', '10 Derecha') = 'Derecha'")
base.final[,"P10ST"] = recode(base.final[,"P10ST"], "c('EstÃ¡ estancado', 'EstÃ¡ en retroceso') = 'No está progresando'")
base.final[,"S1NICC7"] = recode(base.final[,"S1NICC7"], "c('Nada', 'Poco') = 'Poco - Nada'")

base.final[,"P49ST.A"] = recode(base.final[,"P49ST.A"], "c('01 No es democrÃ¡tico', '2', '3') = 'No Democrático';
                              c('4', '5', '6', '7') = 'Ni Demo-Ni No Demo'; c('8', '9', '10 Totalmente democrÃ¡tico') = 'Democrático'")

#base.final[,"P49ST.B"] = recode(base.final[,"P49ST.B"], "c('01 No es democrÃ¡tico', '2', '3') = 'No Democrático';
#                              c('4', '5', '6', '7') = 'Ni Demo-Ni No Demo'; c('8', '9', '10 Totalmente democrÃ¡tico') = 'Democrático'")

#base.final[,"P49ST.E"] = recode(base.final[,"P49ST.E"], "c('01 No es democrÃ¡tico', '2', '3') = 'No Democrático';
#                              c('4', '5', '6', '7') = 'Ni Demo-Ni No Demo'; c('8', '9', '10 Totalmente democrÃ¡tico') = 'Democrático'")

write.arff(base.final, "Base_Final.arff")
