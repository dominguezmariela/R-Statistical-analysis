
A_Factorial_HATCO

install.packages("psych")
library(psych)
install.packages("psy")
library(psy)
install.packages("polycor")
library(polycor)
install.packages("ggcorrplot")
library(ggcorrplot)
library(MVN)

datoH <- read.csv("HATCO2.csv",header=T)
head(datoH)
#Normalidad Multivariada#Se pone el -1 en el corchete para excluír del analisis alguna variable

outliers<-mvn(datoH[,-1],mvnTest="mardia",multivariateOutlierMethod="quan")
outliers
royston<-mvn(datoH[,-1],mvnTest="royston",multivariatePlot="qq")
royston$multivariateNormality

#Los outliers 15 , 32 y 98 se guardan en f y eliminan de la base datoH
f<-c(15,32)
datH<-datoH[-f,-1]
datH
w<-dim(datH)
w
n<-w[1]
p<-w[2]
#Volvemos a probar Normalidad Multivariante

outliers<-mvn(datH,mvnTest="mardia",multivariateOutlierMethod="quan")
outliers
royston<-mvn(datH,mvnTest="royston",multivariatePlot="qq")
royston$multivariateNormality

#Aunque quitamos los outliers, no se cumple la normalidad.

mat_cor <- hetcor(datH)$correlations 
ggcorrplot(mat_cor,type="lower",hc.order = T)

#Verificar que la matriz sea factorizable
cortest.bartlett(mat_cor)

KMO(mat_cor)

scree(mat_cor)
fa.parallel(mat_cor,n,fa="fa",)

# Ejecutar el análisis factorial con 3 factores
model<-factanal(datH,factors = 3,method ="mle",rotation = "varimax")#varimax
model
model$loadings[,1:3]

# Diferencia entre matrices
# Matriz residual
round(mat_cor-(model$loadings[,1:3]%*% t(model$loadings[,1:3]) 
+ diag(model$uniquenesses)),3)

modelo1<-fa(mat_cor,
           nfactors = 3,
           rotate = "none",
           fm="mle") # modelo máxima verosimilitud
modelo1

library()
#Rotaciones
library(GPArotation)
rot<-c("none", "varimax", "quartimax","Promax")
bi_mod<-function(tipo){
biplot.psych(fa(datH,nfactors = 2,fm="minres",rotate = tipo),
main = paste("Biplot con rotación ",tipo),col=c(2,3,4),pch = c(21,18),
group = bfi[,"gender"])  
}
par(mfrow=c(2,2))
sapply(rot,bi_mod)
par(mfrow=c(1,1))

 #GRAFICO DE ARBOL

modelo_varimax<-fa(mat_cor,nfactors = 5,rotate = "varimax",
              fa="minres")
fa.diagram(modelo_varimax)

print(modelo_varimax$loadings,cut=0)