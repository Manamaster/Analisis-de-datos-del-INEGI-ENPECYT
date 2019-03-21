rm(list=ls())
setwd("~")
gc()

install.packages("pscl")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
#library(rpart)
#library(rpart.plot)
library(randomForest)
library(nnet)
library(pscl)

setwd("C:/DataScience08012019/enpecyt_bienal_csv/")

enpecytFull1 <- read.csv("conjunto_de_datos/tr_cbasico3.csv")

#Adaptación de variable de interés
##Deportes
enpecytFull1$IntDep <- ifelse(enpecytFull1$S4P1_1 == "4", yes = "0. Nulo", no = ifelse(
  enpecytFull1$S4P1_1 == "1", yes = "3. Muy grande", no = ifelse(
    enpecytFull1$S4P1_1 == "2", yes = "2. Grande", no = "1. Moderado"
  )
))
##Política
enpecytFull1$IntPol <- ifelse(enpecytFull1$S4P1_2 == "4", yes = "0. Nulo", no = ifelse(
  enpecytFull1$S4P1_2 == "1", yes = "3. Muy grande", no = ifelse(
    enpecytFull1$S4P1_2 == "2", yes = "2. Grande", no = "1. Moderado"
  )
))
##Nuevos inventos
enpecytFull1$IntInventos <- ifelse(enpecytFull1$S4P1_3 == "4", yes = "0. Nulo", no = ifelse(
  enpecytFull1$S4P1_3 == "1", yes = "3. Muy grande", no = ifelse(
    enpecytFull1$S4P1_3 == "2", yes = "2. Grande", no = "1. Moderado"
  )
))
##Ciencias exactas
enpecytFull1$IntCienciasE <- ifelse(enpecytFull1$S4P1_4 == "4", yes = "0. Nulo", no = ifelse(
  enpecytFull1$S4P1_4 == "1", yes = "3. Muy grande", no = ifelse(
    enpecytFull1$S4P1_4 == "2", yes = "2. Grande", no = "1. Moderado"
  )
))
##Ciencias sociales
enpecytFull1$IntCienciasS <- ifelse(enpecytFull1$S4P1_5 == "4", yes = "0. Nulo", no = ifelse(
  enpecytFull1$S4P1_5 == "1", yes = "3. Muy grande", no = ifelse(
    enpecytFull1$S4P1_5 == "2", yes = "2. Grande", no = "1. Moderado"
  )
))
##Espectaculos
enpecytFull1$IntEsp <- ifelse(enpecytFull1$S4P1_6 == "4", yes = "0. Nulo", no = ifelse(
  enpecytFull1$S4P1_6 == "1", yes = "3. Muy grande", no = ifelse(
    enpecytFull1$S4P1_6 == "2", yes = "2. Grande", no = "1. Moderado"
  )
))
##Contaminación
enpecytFull1$IntCont <- ifelse(enpecytFull1$S4P1_7 == "4", yes = "0. Nulo", no = ifelse(
  enpecytFull1$S4P1_7 == "1", yes = "3. Muy grande", no = ifelse(
    enpecytFull1$S4P1_7 == "2", yes = "2. Grande", no = "1. Moderado"
  )
))
##Frecuencia horóscopos
enpecytFull1$HorFreq <- ifelse(enpecytFull1$S4P32_1 == "1", yes = "1. Diariamente", no = ifelse(
  enpecytFull1$S4P32_1 == "2", yes = "2. Frecuentemente", no = "3. Ocasionalmente"
))

#Evaluación de conocimientos científicos
#View(enpecytFull1$S4P18_1)
enpecytFull1$P1 <- ifelse(enpecytFull1$S4P18_1 == 1, yes = 1, no = 0)
enpecytFull1$P2 <- ifelse(enpecytFull1$S4P18_2 == 2, yes = 1, no = 0)
enpecytFull1$P3 <- ifelse(enpecytFull1$S4P18_3 == 1, yes = 1, no = 0)
enpecytFull1$P4 <- ifelse(enpecytFull1$S4P18_4 == 1, yes = 1, no = 0)
enpecytFull1$P5 <- ifelse(enpecytFull1$S4P18_5 == 2, yes = 1, no = 0)
enpecytFull1$P6 <- ifelse(enpecytFull1$S4P18_6 == 1, yes = 1, no = 0)
enpecytFull1$P7 <- ifelse(enpecytFull1$S4P18_7 == 2, yes = 1, no = 0)
enpecytFull1$P8 <- ifelse(enpecytFull1$S4P18_8 == 1, yes = 1, no = 0)
enpecytFull1$P9 <- ifelse(enpecytFull1$S4P18_9 == 2, yes = 1, no = 0)
enpecytFull1$P10 <- ifelse(enpecytFull1$S4P18_10 == 1, yes = 1, no = 0)
enpecytFull1$P11 <- ifelse(enpecytFull1$S4P18_11 == 1, yes = 1, no = 0)
enpecytFull1$P12 <- ifelse(enpecytFull1$S4P18_12 == 2, yes = 1, no = 0)
enpecytFull1$P13 <- ifelse(enpecytFull1$S4P18_13 == 2, yes = 1, no = 0)
enpecytFull1$P14 <- ifelse(enpecytFull1$S4P18_14 == 1, yes = 1, no = 0)
enpecytFull1$P15 <- ifelse(enpecytFull1$S4P18_15 == 2, yes = 1, no = 0)
enpecytFull1$P16 <- ifelse(enpecytFull1$S4P18_16 == 1, yes = 1, no = 0)
enpecytFull1$P17 <- ifelse(enpecytFull1$S4P18_17 == 2, yes = 1, no = 0)
enpecytFull1$P18 <- ifelse(enpecytFull1$S4P18_18 == 2, yes = 1, no = 0)
enpecytFull1$P19 <- ifelse(enpecytFull1$S4P18_19 == 2, yes = 1, no = 0)
enpecytFull1$P20 <- ifelse(enpecytFull1$S4P18_20 == 1, yes = 1, no = 0)
enpecytFull1$P21 <- ifelse(enpecytFull1$S4P19 == 3, yes = 1, no = 0)
enpecytFull1$P22 <- ifelse(enpecytFull1$S4P20 == 3, yes = 1, no = 0)
enpecytFull1$CalifCiencia <- enpecytFull1$P1+enpecytFull1$P2+enpecytFull1$P3+enpecytFull1$P4+enpecytFull1$P5+enpecytFull1$P6+enpecytFull1$P7+enpecytFull1$P8+enpecytFull1$P9+enpecytFull1$P10+enpecytFull1$P11+enpecytFull1$P12+enpecytFull1$P13+enpecytFull1$P14+enpecytFull1$P15+enpecytFull1$P16+enpecytFull1$P17+enpecytFull1$P18+enpecytFull1$P19+enpecytFull1$P20+enpecytFull1$P21+enpecytFull1$P22
  
#Respeto a investigadores/sacerdote
enpecytFull1$S4P14_7[enpecytFull1$S4P14_2 == 11] <- NA
enpecytFull1$S4P14_7[enpecytFull1$S4P14_7 == 11] <- NA
enpecytFull1$S4P14_9[enpecytFull1$S4P14_9 == 11] <- NA
enpecytFull1$S4P14_10[enpecytFull1$S4P14_10 == 11] <- NA
enpecytFull1$S4P14_11[enpecytFull1$S4P14_11 == 11] <- NA
enpecytFull1$S4P14_14[enpecytFull1$S4P14_14 == 11] <- NA
enpecytFull1$S4P14_15[enpecytFull1$S4P14_15 == 11] <- NA
enpecytFull1$S4P14_16[enpecytFull1$S4P14_16 == 11] <- NA
enpecytFull1$S4P14_17[enpecytFull1$S4P14_17 == 11] <- NA
barplot(table(enpecytFull1$S4P14_2)) #Médicos
mean(na.omit(enpecytFull1$S4P14_7))
barplot(table(enpecytFull1$S4P14_7)) #Ingenieros
mean(na.omit(enpecytFull1$S4P14_7))
barplot(table(enpecytFull1$S4P14_9)) #Profesores
mean(na.omit(enpecytFull1$S4P14_9))
barplot(table(enpecytFull1$S4P14_10)) #Deportistas
mean(na.omit(enpecytFull1$S4P14_10))
barplot(table(enpecytFull1$S4P14_11)) #Artistas
mean(na.omit(enpecytFull1$S4P14_11))
barplot(table(enpecytFull1$S4P14_14)) #Policias
mean(na.omit(enpecytFull1$S4P14_14))
barplot(table(enpecytFull1$S4P14_15)) #Sacerdotes
mean(na.omit(enpecytFull1$S4P14_15))
barplot(table(enpecytFull1$S4P14_16)) #Investigadores
mean(na.omit(enpecytFull1$S4P14_16))
barplot(table(enpecytFull1$S4P14_17)) #Inventores
mean(na.omit(enpecytFull1$S4P14_17))

#Creencias pseudocientíficas
##Algunas personas poseen poderes psíquicos
enpecytFull1$S4P31_1_6[enpecytFull1$S4P31_1_6 == 5] <- NA
barplot(table(enpecytFull1$S4P31_1_6))
##Pseudoterapias
enpecytFull1$S4P31_1_7[enpecytFull1$S4P31_1_7 == 5] <- NA
barplot(table(enpecytFull1$S4P31_1_7))
##OVNI
enpecytFull1$S4P31_1_5[enpecytFull1$S4P31_1_5 == 5] <- NA
barplot(table(enpecytFull1$S4P31_1_5))

#Por estados
barplot(table(enpecytFull1$ENT))
table(enpecytFull1$ENT == 9, enpecytFull1$S3P1 ==7)
barplot(table(enpecytLic$ENT, enpecytLic$S4P31_1_5))
table(enpecytFull1$ENT, enpecytFull1$S3P1 ==7)

#Razones por las que la gente no está interesada.
unique(enpecytFull1$S4P1_1_ESA)
table(enpecytFull1$S4P1_1_1A)
table(enpecytFull1$S4P1_1_2A)
table(enpecytFull1$S4P1_1_3A)
table(enpecytFull1$S4P1_1_4A)
table(enpecytFull1$S4P1_1_5A)
table(enpecytFull1$S4P1_1_6A)
table(enpecytFull1$S4P1_1_7A)
table(enpecytSec$S4P1_1_1B)
table(enpecytSec$S4P1_1_2B)
table(enpecytSec$S4P1_1_3B)
table(enpecytSec$S4P1_1_4B)
table(enpecytSec$S4P1_1_5B)
table(enpecytSec$S4P1_1_6B)
table(enpecytSec$S4P1_1_7B)

#Generación de grupos
enpecytEdu <- as.data.frame(enpecytFull1)
enpecytEdu %>% 
  filter(S3P1 %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) %>% 
  filter(S4P10 %in% c("1"))
class(enpecytEdu$S3P1)
enpecytEdu$S3P1 <- as.factor(enpecytEdu$S3P1) 
enpecytEdu$S4P31 <- as.factor(enpecytEdu$S4P31) 
#Quitar a las personas sin acceso a internet
enpecytEdu <- enpecytEdu[enpecytEdu$S4P10=="1", ]
cc1 <- is.na(enpecytEdu$S4P10)
m1 <- which(cc1==c("TRUE"))
enpecytEdu <- enpecytEdu[-m1,]
#Personas sin internet
enpecytSinInt <- as.data.frame(enpecytFull1)
enpecytSinInt <- enpecytSinInt[enpecytSinInt$S4P10=="2", ]
cc <- is.na(enpecytSinInt$S4P10)
m <- which(cc==c("TRUE"))
enpecytSinInt <- enpecytSinInt[-m,]
#Interés por el área físico-matemática y ciencias de la tierra por grado educativo concluido.
# interes1<-ggplot(data=enpecytEdu, aes(x=S3P1, y=S4P1_2_1)) +
#   geom_bar(stat="identity")
# interes1
# #Interés en los deportes por grado escolar concluido
# interesDep<-ggplot(data=enpecytEdu, aes(x=S3P1, y=S4P1_1)) +
#   geom_bar(stat="identity")
# interesDep


#Confianza en la ciencia y en la fe.
enpecytLic <- enpecytEdu[enpecytEdu$S3P1=="7", ]
enpecytLic <- enpecytLic[enpecytLic$S4P31 %in% c("1", "2", "3", "4", "5", "6"), ]
enpecytLic$S4P31 <- ifelse(
  test = enpecytLic$S4P31 == "1",
  yes = "1. Confía más en la ciencia",
  no = ifelse(
    test = enpecytLic$S4P31 == "2",
    yes = "2. Confía más en la religión",
    no = ifelse(
      test = enpecytLic$S4P31 == "3",
      yes = "3. Confía igualmente en ambas",
      no = ifelse(
        test = enpecytLic$S4P31 == "4",
        yes = "4. Confía en su intuición ",
        no = ifelse(
          test = enpecytLic$S4P31 == "5",
          yes = "5. No confía en ninguna",
          no = "6. No sabe."
        )
      )
    )))

barplot(table(enpecytLic$S4P31),main="Confianza en la ciencia y en la religión en personas con licenciatura", col = "#9c27b0")

##Intereses particulares
dev.new()
par(mfrow=c(1,4))
###Área físico-matemáticas
barplot(table(enpecytLic$IntCienciasE), las=2, main = "Nivel de interés en ciencias exactas \n (Licenciatura)", col = "#9c27b0")
###Deportes
barplot(table(enpecytLic$IntDep), las=2, main = "Nivel de interés en los deportes. (Licenciatura)", col = "#9c27b0")
###Ciencias sociales
barplot(table(enpecytLic$IntCienciasS), las=2, main = "Nivel de interés en las ciencias sociales. \n (Licenciatura)", col = "#9c27b0")
###Contaminación
barplot(table(enpecytLic$IntCont), las=2, main = "Nivel de interés en la contaminación. \n (Licenciatura)", col = "#9c27b0")
##Horóscopos
par(mfrow=c(2,1))
barplot(table(enpecytLic$S4P32), main = "Personas que han leído su horóscopo. \n (Licenciatura)", col = "#9c27b0")
horoscoposLic <- as.data.frame(enpecytLic[enpecytLic$S4P32=="1", ])
horoscoposLic$HorFreq <- as.factor(horoscoposLic$HorFreq)
barplot(table(horoscoposLic$HorFreq), main = "Frecuencia de lectura de horóscopos. \n (Licenciatura)", col = "#9c27b0")

#Confianza en figuras de autoridad
dev.new()
par(mfrow=c(4,1))
barplot(table(enpecytLic$S4P14_7)) #Ingenieros
mean(na.omit(enpecytLic$S4P14_7))
barplot(table(enpecytLic$S4P14_15)) #Sacerdotes
mean(na.omit(enpecytLic$S4P14_15))
barplot(table(enpecytLic$S4P14_16)) #Investigadores
mean(na.omit(enpecytLic$S4P14_16))
barplot(table(enpecytLic$S4P14_17)) #Inventores
mean(na.omit(enpecytLic$S4P14_17))
barplot(table(enpecytLic$S4P14_9)) #Profesores
mean(na.omit(enpecytLic$S4P14_9))
barplot(table(enpecytLic$S4P14_11)) #Artistas
mean(na.omit(enpecytLic$S4P14_11))
mean(na.omit(enpecytLic$S4P14_2))

enpecytSec <- enpecytEdu[enpecytEdu$S3P1=="3", ]
enpecytSec <- enpecytSec[enpecytSec$S4P31 %in% c("1", "2", "3", "4", "5", "6"), ]
enpecytSec$S4P31 <- ifelse(
  test = enpecytSec$S4P31 == "1",
  yes = "1. Confía más en la ciencia",
  no = ifelse(
    test = enpecytSec$S4P31 == "2",
    yes = "2. Confía más en la religión",
    no = ifelse(
      test = enpecytSec$S4P31 == "3",
      yes = "3. Confía igualmente en ambas",
      no = ifelse(
        test = enpecytSec$S4P31 == "4",
        yes = "4. Confía en su intuición ",
        no = ifelse(
          test = enpecytSec$S4P31 == "5",
          yes = "5. No confía en ninguna",
          no = "6. No sabe."
        )
      )
    )))
par(mfrow=c(1,1))
barplot(table(enpecytSec$S4P31),main="Confianza en la ciencia y en la religión en personas con secundaria", col = "#2e7d32")

##Intereses particulares
dev.new()
par(mfrow=c(1,4))
###Área físico-matemáticas
barplot(table(enpecytSec$IntCienciasE), las=2, main = "Nivel de interés en ciencias exactas (Secundaria)", col = "#2e7d32")
###Deportes
barplot(table(enpecytSec$IntDep), las=2, main = "Nivel de interés en los deportes. (Secundaria)", col = "#2e7d32")
###Ciencias sociales
barplot(table(enpecytSec$IntCienciasS), las=2, main = "Nivel de interés en las ciencias sociales. (Secundaria)", col = "#2e7d32")
###Contaminación
barplot(table(enpecytSec$IntCont), las=2, main = "Nivel de interés en la contaminación. (Secundaria)", col = "#2e7d32")
##Horóscopos
par(mfrow=c(2,1))
barplot(table(enpecytSec$S4P32), main = "Personas que han leído su horóscopo. \n (Secundaria)", col = "#2e7d32")
horoscoposSec <- as.data.frame(enpecytSec[enpecytSec$S4P32=="1", ])
horoscoposSec$HorFreq <- as.factor(horoscoposSec$HorFreq)
barplot(table(horoscoposSec$HorFreq), main = "Frecuencia de lectura de horóscopos. \n (Secundaria)", col = "#2e7d32")
#Confianza en figuras de autoridad
dev.new()
par(mfrow=c(4,1))
barplot(table(enpecytSec$S4P14_2)) #Médicos
mean(na.omit(enpecytSec$S4P14_2))
barplot(table(enpecytSec$S4P14_7)) #Ingenieros
mean(na.omit(enpecytSec$S4P14_7))
barplot(table(enpecytSec$S4P14_9)) #Profesores
mean(na.omit(enpecytSec$S4P14_9))
barplot(table(enpecytSec$S4P14_11)) #Artistas
mean(na.omit(enpecytSec$S4P14_11))
barplot(table(enpecytSec$S4P14_15)) #Sacerdotes
mean(na.omit(enpecytSec$S4P14_15))
barplot(table(enpecytSec$S4P14_16)) #Investigadores
mean(na.omit(enpecytSec$S4P14_16))
barplot(table(enpecytSec$S4P14_17)) #Inventores
mean(na.omit(enpecytSec$S4P14_17))

enpecytNorm <- enpecytEdu[enpecytEdu$S3P1=="5", ]
enpecytNorm <- enpecytNorm[enpecytNorm$S4P31 %in% c("1", "2", "3", "4", "5", "6"), ]
enpecytNorm$S4P31 <- ifelse(
  test = enpecytNorm$S4P31 == "1",
  yes = "1. Confía más en la ciencia",
  no = ifelse(
    test = enpecytNorm$S4P31 == "2",
    yes = "2. Confía más en la religión",
    no = ifelse(
      test = enpecytNorm$S4P31 == "3",
      yes = "3. Confía igualmente en ambas",
      no = ifelse(
        test = enpecytNorm$S4P31 == "4",
        yes = "4. Confía en su intuición ",
        no = ifelse(
          test = enpecytNorm$S4P31 == "5",
          yes = "5. No confía en ninguna",
          no = "6. No sabe."
        )
      )
    )))
par(mfrow=c(1,1))
barplot(table(enpecytNorm$S4P31),main="Confianza en la ciencia y en la religión en personas con ed. Normal", col = "#3849aa")
##Intereses particulares
dev.new()
par(mfrow=c(1,4))
###Área físico-matemáticas
barplot(table(enpecytNorm$IntCienciasE), las=2, main = "Nivel de interés en ciencias exactas (Ed. Normal)", col = "#3849aa")
###Deportes
barplot(table(enpecytNorm$IntDep), las=2, main = "Nivel de interés en los deportes. (Ed. Normal)", col = "#3849aa")
###Ciencias sociales
barplot(table(enpecytNorm$IntCienciasS), las=2, main = "Nivel de interés en las ciencias sociales. (Ed. Normal)", col = "#3849aa")
###Contaminación
barplot(table(enpecytNorm$IntCont), las=2, main = "Nivel de interés en la contaminación. (Ed. Normal)", col = "#3849aa")
##Horóscopos
par(mfrow=c(2,1))
barplot(table(enpecytNorm$S4P32), main = "Personas que han leído su horóscopo. \n (Ed. Normal)", col = "#3849aa")
horoscoposNorm <- as.data.frame(enpecytNorm[enpecytNorm$S4P32=="1", ])
horoscoposNorm$HorFreq <- as.factor(horoscoposNorm$HorFreq)
barplot(table(horoscoposNorm$HorFreq), main = "Frecuencia de lectura de horóscopos. \n (Ed. Normal)", col = "#3849aa")

#Confianza en figuras de autoridad
barplot(table(enpecytNorm$S4P14_2)) #Médicos
mean(na.omit(enpecytNorm$S4P14_2))
barplot(table(enpecytNorm$S4P14_7)) #Ingenieros
mean(na.omit(enpecytNorm$S4P14_7))
barplot(table(enpecytNorm$S4P14_9)) #Profesores
mean(na.omit(enpecytNorm$S4P14_9))
barplot(table(enpecytNorm$S4P14_11)) #Artistas
mean(na.omit(enpecytNorm$S4P14_11))
barplot(table(enpecytNorm$S4P14_15)) #Sacerdotes
mean(na.omit(enpecytNorm$S4P14_15))
barplot(table(enpecytNorm$S4P14_16)) #Investigadores
mean(na.omit(enpecytNorm$S4P14_16))
barplot(table(enpecytNorm$S4P14_17)) #Inventores
mean(na.omit(enpecytNorm$S4P14_17))


enpecytPost <- enpecytEdu[enpecytEdu$S3P1 %in% c("8", "9", "10"), ]
enpecytPost <- enpecytPost[enpecytPost$S4P31 %in% c("1", "2", "3", "4", "5", "6"), ]
enpecytPost$S4P31 <- ifelse(
  test = enpecytPost$S4P31 == "1",
  yes = "1. Confía más en la ciencia",
  no = ifelse(
    test = enpecytPost$S4P31 == "2",
    yes = "2. Confía más en la religión",
    no = ifelse(
      test = enpecytPost$S4P31 == "3",
      yes = "3. Confía igualmente en ambas",
      no = ifelse(
        test = enpecytPost$S4P31 == "4",
        yes = "4. Confía en su intuición ",
        no = ifelse(
          test = enpecytPost$S4P31 == "5",
          yes = "5. No confía en ninguna",
          no = "6. No sabe."
        )
      )
    )))
par(mfrow=c(1,1))
barplot(table(enpecytPost$S4P31),main="Confianza en la ciencia y en la religión en personas con posgrado", col = "#ff1744")
##Intereses particulares
dev.new()
par(mfrow=c(1,4))
###Área físico-matemáticas
barplot(table(enpecytPost$IntCienciasE), las=2, main = "Nivel de interés en ciencias exactas (Posgrado)", col = "#ff1744")
###Deportes
barplot(table(enpecytPost$IntDep), las=2, main = "Nivel de interés en los deportes. (Posgrado)", col = "#ff1744")
###Ciencias sociales
barplot(table(enpecytPost$IntCienciasS), las=2, main = "Nivel de interés en las ciencias sociales. (Posgrado)", col = "#ff1744")
###Contaminación
barplot(table(enpecytPost$IntCont), las=2, main = "Nivel de interés en la contaminación. (Posgrado)", col = "#ff1744")
##Horóscopos
par(mfrow=c(2,1))
barplot(table(enpecytPost$S4P32), main = "Personas que han leído su horóscopo. \n (Posgrado)", col = "#ff1744")
horoscoposPost <- as.data.frame(enpecytPost[enpecytPost$S4P32=="1", ])
horoscoposPost$HorFreq <- as.factor(horoscoposPost$HorFreq)
barplot(table(horoscoposPost$HorFreq), main = "Frecuencia de lectura de horóscopos. \n (Posgrado)", col = "#ff1744")

#Confianza en figuras de autoridad
barplot(table(enpecytPost$S4P14_2)) #Médicos
mean(na.omit(enpecytPost$S4P14_2))
barplot(table(enpecytPost$S4P14_7)) #Ingenieros
mean(na.omit(enpecytPost$S4P14_7))
barplot(table(enpecytPost$S4P14_9)) #Profesores
mean(na.omit(enpecytPost$S4P14_9))
barplot(table(enpecytPost$S4P14_11)) #Artistas
mean(na.omit(enpecytPost$S4P14_11))
barplot(table(enpecytPost$S4P14_15)) #Sacerdotes
mean(na.omit(enpecytPost$S4P14_15))
barplot(table(enpecytPost$S4P14_16)) #Investigadores
mean(na.omit(enpecytPost$S4P14_16))
barplot(table(enpecytPost$S4P14_17)) #Inventores
mean(na.omit(enpecytPost$S4P14_17))

#Comparación entre personas con y sin internet
par(mfrow=c(2,2))
barplot(table(enpecytEdu$S4P32), main = "Personas que han leído su horóscopo.", col = "#ffc400")
horoscoposI <- as.data.frame(enpecytEdu[enpecytEdu$S4P32=="1", ])
horoscoposI$HorFreq <- as.factor(horoscoposI$HorFreq)
barplot(table(horoscoposI$HorFreq), main = "Frecuencia de lectura de horóscopos.", col = "#ffc400")

barplot(table(enpecytSinInt$S4P32), main = "Personas que han leído su horóscopo. \n (Sin internet)", col = "#00b0ff")
horoscoposSI <- as.data.frame(enpecytSinInt[enpecytSinInt$S4P32=="1", ])
horoscoposSI$HorFreq <- as.factor(horoscoposSI$HorFreq)
barplot(table(horoscoposSI$HorFreq), main = "Frecuencia de lectura de horóscopos.  \n (Personas sin internet)", col = "#00b0ff")
#Nivel educativo de las personas con y sin acceso a internet.
par(mfrow=c(2,1))
barplot(table(enpecytSinInt$S3P1), main = "Nivel educativo de personas sin internet.", col = "#00b0ff")
barplot(table(enpecytEdu$S3P1), main = "Nivel educativo de personas con acceso a internet.", col = "#ffc400")

#Conocimiento de ciencia
dev.new()
par(mfrow = c(1,4))
barplot(table(enpecytSec$CalifCiencia),main="calificación de ciencia (Min: 0 Max: 22) \n (Secundaria)", col = "#2e7d32")
abline(h = 1)
barplot(table(enpecytNorm$CalifCiencia),main="calificación de ciencia (Min: 0 Max: 22) \n (Ed. Normal)", col = "#3849aa")
abline(h = 1)
barplot(table(enpecytLic$CalifCiencia),main="calificación de ciencia (Min: 0 Max: 22) \n (Licenciatura)", col = "#9c27b0")
abline(h = 1)
barplot(table(enpecytPost$CalifCiencia),main="calificación de ciencia (Min: 0 Max: 22) \n (Posgrado)", col = "#ff1744")
abline(h = 60)
abline(h = 1)
moda(enpecytSec$CalifCiencia)
moda(enpecytNorm$CalifCiencia)
moda(enpecytLic$CalifCiencia)
moda(enpecytPost$CalifCiencia)

#Conocimientos básicos de ciencia
barplot(table(enpecytLic$S4P18_7), main = "Gente que cree que los antibióticos tratan virus. \n (Licenciatura)", col = "#9c27b0")
abline(h=20)
barplot(table(enpecytLic$S4P18_12), main = "Gente que cree que los humanos \n convivieron con dinosaurios. \n (Licenciatura)", col = "#9c27b0")
barplot(table(enpecytLic$S4P18_13), main = "Gente que cree que hay Nobel de matemáticas. \n (Licenciatura)", col = "#9c27b0")
barplot(table(enpecytLic$S4P18_5), main = "Gente que cree que los rayos láser \n funcionan con ondas sonoras. \n (Licenciatura)", col = "#9c27b0")
barplot(table(enpecytLic$S4P18_6), main = "Gente que cree que los electrones son más pequeños que los átomos. \n (Licenciatura)", col = "#9c27b0")

barplot(table(enpecytLic$S4P20), main = "Pruebas de medicamentos. \n (Licenciatura)", col = "#9c27b0")
barplot(table(enpecytSec$S4P19), main = "Posibilidad de que un hijo nazca con una \n enfermedad congénita. \n (Secundaria)", col = "#2e7d32")


barplot(table(enpecytSec$S4P18_7), main = "Gente que cree que los antibióticos tratan virus. \n (Secundaria)", col = "#2e7d32")
barplot(table(enpecytSec$S4P18_12), main = "Gente que cree que los humanos \n convivieron con dinosaurios. \n (Secundaria)", col = "#2e7d32")

#Creencias en la pseudociencia
table(enpecytFull1$S4P31_1_7)

#barplot(table(enpecytEdu$S3P1,enpecytSinInt$S3P1), col = c("#ffc400", "#00b0ff"), main = "Nivel educativo de personas \n con y sin internet.", beside = TRUE)

#Hacer analisis factorial, que es como componentes principales para categóricas.
#Modelo
# arbol1 <- rpart(enpecytFull1$S4P31~ S4P1_1 + S4P1_2 + S4P1_3 + S4P1_4 + S4P1_5 + S4P1_6 + S4P1_7 +
#                   S4P32 + S4P32_1 + S4P10, data = enpecytFull1)
# rpart.plot(arbol1)
# 
# arbol2 <- rpart(S4P31~ S4P1_1 + S4P1_2 + S4P1_3 + S4P1_4 + S4P1_5 + S4P1_6 + S4P1_7 +
#                   S4P32 + S4P32_1 + S4P10, data = enpecyt_train)
# rpart.plot(arbol2)

set.seed(369)
index <- sample( x = nrow(enpecytFull1), size = .8*nrow(enpecytFull1), replace = FALSE)
head(index)
nrow(enpecytFull1)
enpecyt_train <- enpecytFull1[index, ]
enpecyt_validate <- enpecytFull1[-index, ]

# cc2 <- is.na(enpecyt_train$S4P31)
# m2 <- which(cc2==c("TRUE"))
# enpecyt_train <- enpecyt_train[-m2,]

#Random Forest (No sirve por el tamaño de la muestra para predecir, pero sí para determinar la significancia de
# las variables.)
enpecyt_RF <- randomForest(S4P31 ~ S4P1_1 + S4P1_2 + S4P1_3 + S4P1_4 + S4P1_5 + S4P1_6 + S4P1_7 +
                             S4P32 + S4P32_1 + S4P10 + S3P1 + CalifCiencia + S4P14_1 + S4P14_2 + S4P14_15 + 
                             S4P14_16 + S4P14_16 + S4P14_7 + S4P14_8 + S4P14_9 + S4P31_1_7 + S4P31_1_6 + S4P31_1_5, 
                           data = enpecytFull1, ntree = 500, importance = TRUE, na.action=na.roughfix)
enpecyt_RF
importance(enpecyt_RF)        
varImpPlot(enpecyt_RF)

#Multinomial
enpecyt_MN <- glm(formula = S4P31 ~ S4P1_1 + S4P1_2 + S4P1_3 + S4P1_4 + S4P1_5 + S4P1_6 + S4P1_7 +
                    S4P32 + S4P32_1 + S4P10 + S3P1 + CalifCiencia + S4P14_1 + S4P14_2 + S4P14_15 + 
                    S4P14_16 + S4P14_16 + S4P14_7 + S4P14_8 + S4P14_9 + S4P31_1_7 + S4P31_1_6 + S4P31_1_5, 
                  data = enpecytFull1)
summary(enpecyt_MN)
#Conversión a odd ratios usando la constante de Euler
coefs <- coef(enpecyt_MN)
coefs
exp(coefs)
##Cuando el Exp(b) es mayor de 1 señala que un aumento de la variable independiente, aumenta los odds que ocurra el evento.
#Pseudo R^2 (McFadden)
pR2(enpecyt_MN)
##Cambio porcentual en el incremento de una unidad en la variable dependiente
(exp(coefs)-1)*100

#Multinomial2
enpecytFull1$S4P31 <- as.factor(enpecytFull1$S4P31)
enpecytFull1$S4P31 <- relevel(enpecytFull1$S4P31, ref = 1)
enpecyt_MN1 <- multinom(formula = S4P31 ~ S4P1_1 + S4P1_2 + S4P1_3 + S4P1_4 + S4P1_5 + S4P1_6 + S4P1_7 +
                          S4P32 + S4P32_1 + S4P10 + S3P1 + CalifCiencia + S4P14_1 + S4P14_2 + S4P14_15 + 
                          S4P14_16 + S4P14_16 + S4P14_7 + S4P14_8 + S4P14_9 + S4P31_1_7 + S4P31_1_6 + S4P31_1_5, 
                        data = enpecyt_train)
summary(enpecyt_MN1)
##Obtener Z-value
zvalues <- summary(enpecyt_MN1)$coefficients / summary(enpecyt_MN1)$standard.errors
zvalues
##Obtener p-value
pnorm(abs(zvalues), lower.tail=FALSE)*2

#Logística
enpecytFull1$ConfiaCiencia <- ifelse(enpecytFull1$S4P31 == 1, yes = 1, no = 0)
enpecyt_LM <- glm(formula = ConfiaCiencia ~ S4P1_1 + S4P1_2 + S4P1_3 + S4P1_4 + S4P1_5 + S4P1_6 + S4P1_7 +
                    S4P32 + S4P32_1 + S4P10 + S3P1 + CalifCiencia + S4P14_1 + S4P14_2 + S4P14_15 + 
                    S4P14_16 + S4P14_16 + S4P14_7 + S4P14_8 + S4P14_9 + S4P31_1_7 + S4P31_1_6 + S4P31_1_5, 
                  data = enpecyt_train, family = "binomial")
summary(enpecyt_LM)
#Conversión a odd ratios usando la constante de Euler
coefs1 <- coef(enpecyt_LM)
coefs1
exp(coefs1)
##Cuando el Exp(b) es mayor de 1 señala que un aumento de la variable independiente, aumenta los odds que ocurra el evento.
#Pseudo R^2 (McFadden)
pR2(enpecyt_LM)
##Cambio porcentual en el incremento de una unidad en las variables independientes
(exp(coefs1)-1)*100

#Comparación de modelos
pR2(enpecyt_LM)
pR2(enpecyt_MN)  #GLM
pR2(enpecyt_MN1) #Multinom

#Predicciones
enpecyt_validate$pred_LM <- predict(object = enpecyt_LM, newdata = enpecyt_validate, type = "response")
enpecyt_validate$pred_MN1 <- predict(object = enpecyt_MN1, newdata = enpecyt_validate, type = "class")

##Exactitud de las predicciones
enpecyt_validate$pred_LM <- ifelse(enpecyt_validate$pred_LM > .5, yes=0, no=1)
rate_LM <- mean(enpecyt_validate$ConfiaCiencia == enpecyt_validate$pred_LM)
rate_LM
#Funciones
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
barplot(table(enpecytFull1$S4P31_1_6), main = "Hay personas con poderes psíquicos \n (Población general)", col = "#6a1b9a")


