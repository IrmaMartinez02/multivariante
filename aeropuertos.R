
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

plot(aeropuertos2,col="aquamarine3",main="Aeropuertos")


#--------------------------------------
#  PCA sintetizado
#-------------------------------------
x<-as.data.frame(aeropuertos2)


View(x)
head(x)

# Aplicar el cálculo de la varianza a las columnas 
# 1=filas, 2=columnas
apply(x, 2, var)

# centrado por la media y escalada por 
# la desviacion standar (dividir entre sd).

acp<-prcomp(x, center=TRUE, scale=TRUE)
acp

# Generación del gráfico screeplot
plot(acp, type="1")

# Visualizar el resumen
summary(acp)

# Construcción del Biplot
biplot(acp, scale=0)

# Componente principal calculada
# Suma del producto de la matriz acp de cada uno
# de los componentes por el dato de la matriz original
# por filas
# filas = 1
# columnas = 2
pc1<-apply(acp$rotation[,1]*x, 1, sum)
pc2<-apply(acp$rotation[,2]*x, 1, sum)
pc3<-apply(acp$rotation[,3]*x, 1, sum)

x$pc1<-pc1
x$pc2<-pc2
x$pc3<-pc3








