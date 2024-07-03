#Una vez descargados los datos de ECOBICI los leemos en R
BICI2014<-read.csv("2014-02.csv", header = T, sep = ",")
BICI2019<-read.csv("2019-02.csv", header = T, sep = ",")

#Se seleccionan aquellos datos no correspondientes a ECOBICI
BICI2014<-subset(BICI2014, BICI2014$Ciclo_Estacion_Retiro<1000 &
                   BICI2014$Ciclo_Estacion_Arribo<1000)
BICI2019<-subset(BICI2019, BICI2019$Ciclo_Estacion_Retiro<1000 &
                   BICI2019$Ciclo_Estacion_Arribo<1000)

'Se crea una variable que determine el tiempo trascurrido entre la hora de retiro y
la hora de arribo del usaurio'
v1<-BICI2014$Ciclo_Estacion_Retiro==BICI2014$Ciclo_Estacion_Arribo
v1<-v1==FALSE
BICI2014<-BICI2014[v1,]
tiempo1.1<-paste(BICI2014$Fecha_Retiro, BICI2014$Hora_Retiro)
tiempo2.1<-paste(BICI2014$Fecha_Arribo, BICI2014$Hora_Arribo)
tiempo1.1<-as.POSIXct(tiempo1.1, format = '%d/%m/%Y %I:%M:%S %p')
tiempo2.1<-as.POSIXct(tiempo2.1, format = '%d/%m/%Y %I:%M:%S %p')
tiempot.1<-difftime(tiempo2.1,tiempo1.1, units = "mins")
v2<-BICI2019$Ciclo_Estacion_Retiro==BICI2019$Ciclo_Estacion_Arribo
v2<-v2==FALSE
BICI2019<-BICI2019[v2,]
tiempo1.2<-paste(BICI2019$Fecha_Retiro, BICI2019$Hora_Retiro)
tiempo2.2<-paste(BICI2019$Fecha_Arribo, BICI2019$Hora_Arribo)
tiempo1.2<-as.POSIXct(tiempo1.2, format = '%d/%m/%Y %H:%M:%S')
tiempo2.2<-as.POSIXct(tiempo2.2, format = '%d/%m/%Y %H:%M:%S')
tiempot.2<-difftime(tiempo2.2,tiempo1.2, units = "mins")

'De esta manera, se obtienen a solo los trayectos comunes de ECOBICI (es el tiempo
limite de uso de una bicicleta)'
BICI2014<-data.frame(BICI2014, Tiempo_T=tiempot.1)
BICI2014<-subset(BICI2014, BICI2014$Tiempo_T<=45 & BICI2014$Tiempo_T>=0)
BICI2019<-data.frame(BICI2019, Tiempo_T=tiempot.2)
BICI2019<-subset(BICI2019, BICI2019$Tiempo_T<=45 & BICI2019$Tiempo_T>=0)

#Se gráfica está información.
plot(BICI2014$Ciclo_Estacion_Retiro,BICI2014$Ciclo_Estacion_Arribo, col = "lightblue",
     pch="*", xlab = "Estación de retiro", ylab = "Estación de arribo",
     main = "Relación entre cicloestaciones de arribo y llegada febrero 2014")
plot(BICI2019$Ciclo_Estacion_Retiro,BICI2019$Ciclo_Estacion_Arribo, col = "pink",
     pch="*", xlab = "Estación de retiro", ylab = "Estación de arribo",
     main = "Relación entre cicloestaciones de arribo y llegada febrero 2019")

#Se genera un histograma con el cual se ve que el flujo de enrada y salida de
#bicicletas es prácticamente igual.
hist(BICI2014$Ciclo_Estacion_Arribo, breaks = 200, col="red",
     main = "Histograma de cicloestaciones de Arribo 2014", xlab = "Cicloestación",
     ylab = "Frecuencia")
hist(BICI2014$Ciclo_Estacion_Retiro, breaks = 200, col="green",
     main = "Histograma de cicloestaciones de Retiro 2014", xlab = "Cicloestación",
     ylab = "Frecuencia")
hist(BICI2019$Ciclo_Estacion_Arribo, breaks = 400, col="red",
     main = "Histograma de cicloestaciones de Arribo 2019", xlab = "Cicloestación",
     ylab = "Frecuencia")
hist(BICI2019$Ciclo_Estacion_Retiro, breaks = 400, col="green",
     main = "Histograma de cicloestaciones de Retiro 2019", xlab = "Cicloestación",
     ylab = "Frecuencia")

#Los valores de las cicloestaciones no son continuos, por lo cual, se transforman a
#discretos
BICI2014$Ciclo_Estacion_Retiro<-as.factor(BICI2014$Ciclo_Estacion_Retiro)
BICI2014$Ciclo_Estacion_Arribo<-as.factor(BICI2014$Ciclo_Estacion_Arribo)
BICI2019$Ciclo_Estacion_Retiro<-as.factor(BICI2019$Ciclo_Estacion_Retiro)
BICI2019$Ciclo_Estacion_Arribo<-as.factor(BICI2019$Ciclo_Estacion_Arribo)

#Se generan vectores que indican que cicloestaciones son más usadas.
masflujo.2014_retiro<-summary(BICI2014$Ciclo_Estacion_Retiro)
masflujo.2014_arribo<-summary(BICI2014$Ciclo_Estacion_Arribo)
masflujo.2019_retiro<-summary(BICI2019$Ciclo_Estacion_Retiro)
masflujo.2019_arribo<-summary(BICI2019$Ciclo_Estacion_Arribo)

'Se extraen manualmente aquellos con al menos 5000 en usuarios arribando y
5000 retirando una ecobici en el año 2014
Para el año 2019 este número cambia a 4000'
masflujo.2014_retiro<-c(27, 18, 41, 36, 43, 1, 64, 21, 182, 25, 24)
masflujo.2014_arribo<-c(27, 43, 1, 64, 18, 36, 182, 266, 41, 29, 21, 31)
masflujo.2019_retiro<-c(271, 27, 1, 182, 43, 18, 266, 36, 64, 28, 41)
masflujo.2019_arribo<-c(266, 1, 27, 182, 43, 271, 64, 36, 18, 267, 28, 15, 47, 38)

masflujo.2014<-masflujo.2014_retiro[masflujo.2014_arribo%in%masflujo.2014_retiro]
masflujo.2019<-masflujo.2019_retiro[masflujo.2019_arribo%in%masflujo.2019_retiro]
masflujo.2014<-as.character(masflujo.2014)
masflujo.2019<-as.character(masflujo.2019)

#Se seleccionan los arcos que solo están relacionados con los que tienen mayor flujo
ECOBICI2014<-subset(BICI2014, BICI2014$Ciclo_Estacion_Retiro%in%masflujo.2014 | 
                      BICI2014$Ciclo_Estacion_Arribo%in%masflujo.2014)
ECOBICI2019<-subset(BICI2019, BICI2019$Ciclo_Estacion_Retiro%in%masflujo.2019 | 
                      BICI2019$Ciclo_Estacion_Arribo%in%masflujo.2019)

#Se generan archivos para analiazarlos en redes
write.csv(ECOBICI2014, "ECOBICI2014.csv")
write.csv(ECOBICI2019, "ECOBICI2019.csv")

#Se obtienen las cicloestaciones con menor flujo en ambos años
#Corte en 800
f1<-c(105, 264, 262, 240, 93, 101, 185, 273, 268, 275, 94, 269, 100, 102, 109, 89,
      176, 220, 91)
f2<-c(156, 101, 243, 220, 268, 185, 100, 94, 269, 240, 193, 275, 188, 168, 264, 273,
      218, 105, 262)
f<-f1%in%f2
f<-f1[f]
EBICI2014<-subset(BICI2014, BICI2014$Ciclo_Estacion_Retiro%in%f | 
                      BICI2014$Ciclo_Estacion_Arribo%in%f)
#Corte en 600
f3<-c(407, 376, 479, 429, 408, 406, 441, 469, 388, 445, 457, 403, 411, 410, 262)
f4<-c(428, 101, 416, 102, 275, 410, 435, 360, 414, 346, 361, 421, 349, 339, 399, 296,
  387, 451, 94, 431, 424, 472, 348, 345, 311, 466, 279, 388, 300, 379, 418, 413,
  244, 409, 397, 354, 213, 454, 175, 415, 443, 353, 391, 344, 220, 222, 459, 105,
  417, 453, 403, 375, 203, 408, 318, 241, 362, 423, 467, 289, 366, 247, 199, 445,
  283, 464, 367, 355, 426, 434, 330, 438, 160, 457, 406, 333, 343, 381, 382, 437,
  401, 350, 407, 422, 383, 456, 368, 327, 376, 444, 479, 100, 378, 364, 155, 429,
  332, 411, 380, 469, 441, 262)
f5<-f3[f3%in%f4]
EBICI2019<-subset(BICI2019, BICI2019$Ciclo_Estacion_Retiro%in%f5 | 
                    BICI2019$Ciclo_Estacion_Arribo%in%f5)

#Se sellecionan los usuario que hicieron uso de las cicloestaciones menos usadas
#Se generan dos archivos con está información para cada año
write.csv(EBICI2014, "EBICI2014.csv")
write.csv(EBICI2019, "EBICI2019.csv")

'En Gephi se leyeron los archivos y se obtiene la cantidad de rutas que se generan
a lo largo del mes de una cicloestación a otra. Finalmente, se crearon dos documentos
que permiten analizar está información '

#Se leen los archivos generados en Gephi
ECO_BICI2014<-read.csv("Proyecto ECO_aristas2014.csv", header = T, sep = ",")
ECO_BICI2019<-read.csv("Proyecto ECO_aristas2019.csv", header = T, sep = ",")

#Se generan dos grupos, las rutas que más se realizaron en Febrero 2014
a<-ECO_BICI2014$Source==ECO_BICI2014$Target
a<-a==FALSE
ECO_BICI2014<-ECO_BICI2014[a,]

#Se generan dos grupos, las rutas que más se realizaron en Febrero 2019
a<-ECO_BICI2019$Source==ECO_BICI2019$Target
a<-a==FALSE
ECO_BICI2019<-ECO_BICI2019[a,]

#Se importa la librería ggplot2 para ver estás asociación.
library(ggplot2)
ggplot(data = ECO_BICI2014, aes(x = Source, y = Target, color = Weight,
                                alpha = 0.5)) + geom_point() +
  scale_color_gradientn(colours = rainbow(5)) +
  ggtitle("Dispersión de rutas febrero 2014") + 
  labs(x="Cicloestación", y = "Cicloestación")
ggplot(data = ECO_BICI2019, aes(x = Source, y = Target, color = Weight,
                                alpha = 0.5)) + geom_point() +
  scale_color_gradientn(colours = rainbow(5)) +
  ggtitle("Dispersión de rutas febrero 2019") + 
  labs(x="Cicloestación", y = "Cicloestación")
