### Script de organização dos dados .csv para uso no modelo

data <- read.csv("./parcial.csv", sep= ";", na.strings = c("NA", "na", "", "-", " "))
names(data)

## data inicial

data$dataPrimSintomas <- as.Date(data$DT_SIN_PRI, format="%d/%m/%Y")
data$dataEntradaHosp <- as.Date(data$DT_INTERNA, format="%d/%m/%Y")
data$dataNotificacao <- as.Date(data$DT_NOTIFIC, format="%d/%m/%Y")
data$dataDigitacao <- as.Date(data$DT_DIGITA, format="%d/%m/%Y")

## Diferença
data$atrasoDias <- data$dataNotificacao - data$dataPrimSintomas
matrizDias <- as.matrix(table(data$dataPrimSintomas, data$atrasoDias))
matrizDias

data$atrasoSemanas <- floor( data$atrasoDias / 7 )
matrizSemanas <- as.matrix(table(data$SEM_NOT, data$atrasoSemanas))
matrizSemanas

## Totais
addmargins(matrizDias)
addmargins(matrizSemanas)


###### A partir daqui, rascunho
### Supondo que as datas sejam valores numéricos, em semanas:

data$ATRASO <- data$DT_NOTIFIC - data$DT_INTERNA

maxAtraso <- max(data$ATRASO)
minAtraso <- 0

tempoFinal <- max(data$DT_NOTIFIC)
tempoInicial <- min(data$DT_NOTIFIC)

table(data$ATRASO, data$DT_INTERNA)


##

table(data$DT_NOTIF, data$INTERNA)

###########################
