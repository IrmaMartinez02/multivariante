#Descarga de paquetes y librerías 
install.packages("psych")
library(psych)

install.packages("polycor")
library(polycor)

install.packages("ggcorrplot")
library(ggcorrplot)

# Extracción de datos.
x<-bfi

# Exploración de la matriz.
dim(x)

# Tipo de variables.
str(x)

#Nombre de las variables
colnames(x)

# Creación de una nueva matriz de datos donde se incluyen las variables 1 a la 25 y las primeras 200 observaciones.
x1<-bfi[1:200,1:25]

# Matriz de correlaciones
R<-hetcor(x1)$correlations

# Gráfico de correlaciones
ggcorrplot(R, type = "lower", hc.order=TRUE)

#Factorización de la matriz de correlaciones
#Se utiliza la prueba de esfericidad de Bartlett.

p_Bartlett<-cortest.bartlett(R)


#Visualización del p-valor
p_Bartlett$p.value

# Criterio Kaiser-Meyer-Olkin
KMO(R)

# Extracción de factores 
modelo1<-fa(R,nfactor=3,rotate = "none",fm="mle")

modelo2<-fa(R,nfactor=3,rotate = "none",fm="minres")

C1<-sort(modelo1$communality, decreasing = TRUE)

C2<-sort(modelo2$communality, decreasing = TRUE)

head(cbind(C1,C2))

#Extracción de unidades

u1<-sort(modelo1$uniquenesses, decreasing = TRUE)

u2<-sort(modelo2$uniquenesses, decreasing = TRUE)

head(cbind(u1,u2))

scree(R)

# Rotación de la matriz

install.packages("GPArotation")
library(GPArotation)

rot<-c("None", "Varimax", "Quartimax", "Promax")
bi_mod<-function(tipo){
  biplot.psych(fa(x1, nfactors = 2,  
                  fm= "minres", rotate=tipo),
               main = paste("Biplot con rotación", tipo),
               col=c(2,3,4), pch=c(21,18), group=bfi[,"gender"])
}
sapply(rot,bi_mod)

# Interpretación 
modelo_varimax<-fa(R,nfactor=5,
                   rotate = "varimax",
                   fm="minres")

fa.diagram(modelo_varimax)

#Visualización de la matriz de carga rotada.
print(modelo_varimax$loadings, cut=0)
