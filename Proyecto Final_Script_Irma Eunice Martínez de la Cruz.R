## EXPLORACIÓN DE LA MATRIZ

Se carga la base de datos
install.packages("knitr")
library(knitr)

library(readxl)
Marvel_DC <- read_excel("Marvel_DC.xlsx")
kable(head(Marvel_DC))

## Dimensión:
dim(Marvel_DC)

## Nombre de las variables 
colnames(Marvel_DC)

## Tipo de variables
str(Marvel_DC)

## Presencia de NA´s.
anyNA(Marvel_DC)

#1.- Se convierte la base de datos en un data.frame para convertirla en una matriz 
x<-data.frame(Marvel_DC)

#2.- Se crea una nueva matriz de datos donde se incluyen las variables 4,5,7,9,10 y 11 las cuales son númericas, mientras que se toman las 39 observaciones.
x1<-x[1:39,(c(4,5,7,9,10,11))]

#3.- Se cargan las librerias para los graficos
install.packages("psych")
library(psych)

install.packages("polycor")
library(polycor)

install.packages("ggcorrplot")
library(ggcorrplot)

#4.- Matriz de correlaciones
R<-hetcor(x1)$correlations


#5.- Gráfico de correlaciones
ggcorrplot(R, type="lower", hc.order=TRUE)


#6.- Se utiliza la prueba de esfericidad de Bartlett
p_Bartlett<-cortest.bartlett(R)

#Visualización del p-valor
p_Bartlett$p.value

#Ho:Las variables estan relacionadas
#Ha:Las variables no están correlacionadas.
#Para esta ocasión, No se rechaza Ho, ya que las variables estan correlacionadas.

#7.-Criterio Kaiser-Meyer.Olkin
Se identifica si los datos analizados son adecuados para el analisis factorial,
#>0.00 a 0.49-No adecuados
#>0.50 a 0.59-Poco adecuados
#>0.60 a 0.69-Aceptables
#>0.70 a 0.89-Buenos
#>0.90 a 1.00-Excelentes

KMO(R)

##Generalizando, son buenos los datos para hacer un analisis factorial.

#8.- Extracción de los factores con el máximo de verosimilitud y el minimo residuo
modelo1<-fa(R, nfactor=3, rotate="none", fm="mle")
modelo2<-fa(R, nfactor=3, rotate="none", fm="minres")

#9.- Se extrae el resultado de las comunidalidades, donde se encuentra la varianza explicada, para extraer la mejor variable.
C1<-sort(modelo1$communality, decreasing=TRUE)
C2<-sort(modelo2$communality, decreasing=TRUE)

head(cbind(C1,C2))

#10.- Se extraen las unicidades, lo cual es el cuadro del coeficiente, del factor unico.
u1<-sort(modelo1$uniquenesses, decreasing = TRUE)
u2<-sort(modelo2$uniquenesses, decreasing = TRUE)

head(cbind(u1,u2))

#11.- Scree plot
scree(R)

#12.- Rotación de la matrizrepresentada con matriz
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

#13.- Interpretación 

modelo_varimax<-fa(R,nfactor=5,
                   rotate = "varimax",
                   fm="minres")

fa.diagram(modelo_varimax)

#14.- Visualización de la matriz de carga rotada.
print(modelo_varimax$loadings, cut=0)
