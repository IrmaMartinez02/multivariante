
#PASO 1: INSTALACIÓN DEL PAQUETE DATOS
install.packages("datos")

#PASO 2: ABRIR LIBRERIA
library(datos)

#PASO 3: ELIGE UNA MATRIZ
aeropuertos1<-datos::aeropuertos

#PASO 4:EXPLORACIÓN DE LA MATRIZ
str(aeropuertos1)

#PASO 5:CONFIGURACIÓN Y/O FILTRADO DE VARIABLES (ME QUEDO CON LAS CUANTITATIVAS)
colnames(aeropuertos1)

aeropuertos2<-aeropuertos1[,3:6]
colnames(aeropuertos2)

#PASO 6: DESARROLLAR EL PCA PASO A PASO


#Analisis de componentes principales
x<-as.data.frame(aeropuertos2)
#2.- Quitar los espacios de los nombres
colnames(x)[3]="latitud"
colnames(x)[4]= "longitud"

plot(aeropuertos2,col="aquamarine3",main="Aeropuertos")






