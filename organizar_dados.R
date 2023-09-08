### Script de organização dos dados .csv para uso no modelo

data <- read.csv("./INFLUD21-01-05-2023.csv", sep= ";")
names(data)

## data inicial

dataPrimSintomas <- "DT_SIN_PRI"
dataEntradaHosp <- "DT_INTERNA"
dataNotificacao <- "DT_NOTIFIC"

## data de digitação

dataDig <- "DT_DIGITA"

## organizar dados

source("fixdata.R")


### Supondo que as datas sejam valores numéricos, em semanas:

data$ATRASO <- data$DT_NOTIFIC - data$DT_INTERNA

maxAtraso <- max(data$ATRASO)
minAtraso <- 0

tempoFinal <- max(data$DT_NOTIFIC)
tempoInicial <- min(data$DT_NOTIFIC)

table(data$ATRASO, data$DT_INTERNA)


##



###########################
