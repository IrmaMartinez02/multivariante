# ANALISIS DISCRIMINANTE LINEA
library(MASS)

# Se cargan los datos iris
Z<-as.data.frame(cats)
Z
# Se define la matriz de datos y la variable
x<-Z[,2:3]
y<-Z[,1]

# Definir como n y p el numero de gatos y variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el Analisis discriminante lineal (LDA)
# Cross validation (cv): clasificacion optima
lda.cats<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas por CV usando LDA.
lda.cats$class

# Creacion de la tabla de clasificaciones buenas y malas
table.cats<-table(y,lda.cats$class)
table.cats

# Proporcion de errores
mis.cats<- n-sum(y==lda.cats$class)
mis.cats/n

# scater plot
# Buenas clasificaciones en amarillo y malas en verde
col.lda.cats<-c("green","yellow")[1*(y==lda.cats$class)+1]
pairs(x,main="Buena Clasificacion (Amarillo), Mala Clasificacion (verde)",
      pch=19,col=col.lda.cats)


# Probabilidad de pertenencia a uno de los dos grupos
lda.cats$posterior

# Grafico de probabilidades
plot(1:n, lda.cats$posterior[,1],
     main="Probabilidades a posterior",
     pch=20, col="cyan",
     xlab="Numero de observaciones", ylab="Probabilidades")
points(1:n,lda.cats$posterior[,2],
       pch=20, col="green")

