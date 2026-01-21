plot(rnorm(15, 10, 5),col = "red", type = "l")
lines(rnorm(15, 10, 5), col= "blue", type = "p", pch=1)
lines(rnorm(15, 10, 5), col= "green", type = "b", pch=2)
title(main = "mi grafico")

file.choose()
#iris
#attach(iris)
boxplot(Sepal.Length~Species,ylab = "Sepal.Lenght")
boxplot(x=iris[,1:4], main="Boxplot Iris")
#El ancho del sepalo Vs el largo del sepalo
plot(Sepal.Width~Sepal.Length, col=Species)
#Equivalente
plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species))
#Le agregamos una leyenda
legend("topright", levels(Species), lty = 1, col=1:3, bty = "n", cex = .75)

pairs(iris[,1:4], pch=as.numeric(iris$Species), col=iris$Species)
install.packages("scatterplot3d", dependencies = T)
library(scatterplot3d) 
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width, 
              color = as.numeric(iris$Species), pch = as.numeric(iris$Species))
parcoord(iris[1:4], col=iris$var.label=T)
iris_sample1<- iris[sample(1:dim(iris)[1],size = 6, replace = F),]
rownames(iris_sample1)<- paste(as.character(iris_sample1$Species), 1:6)
stars(iris_sample1[1:4])

install.packages("aplpack")
library(aplpack)

iris_sample <- iris[sample(1:dim(iris)[1], size = 16, replace = F),]
faces(iris_sample[1:4],face.type = 1, labels = iris_sample$Species)

#base de datos generada en el aula
setwd("/Users/marieladominguezdominguez/Documents")
file.choose()
data <- read.table("/Users/marieladominguezdominguez/Multivariado.csv", header = TRUE)
data()
datoEME <- read.csv(file = "Multivariado.csv", header = TRUE)
datoE<- datoEME[,-1]
datoE

#Asignar nombres a los renglones

rownames(datoE)<- ("Juliet", "Itzel", "Marely", "Josué", "Mariela", "Diego", "Dominique", "Jesús", "Yair", "Antonio", "Xiomara", "Melissa", "Viviana", "Arantza", "Dulce", "Isabel", "Miguel", "Karla", "Arleth", "Cesar", "Marisol", "Luís", 
                   "Yahir")
faces(datoE)
stars(datot)
attach(datoE)
plot(Edad, Efectivo)
pairs(datoE)

install.packages("andrews")
library(andrews)
andrews(datoE)

#Codigo anterior version nueva
# Base de datos de los alumnos generada en el aula.
file.choose()
setwd("C:/Users/marieladominguezdominguez/Desktop/Multivariado.csv")
datoEME <- read.csv("Multivariado.csv", header=TRUE)
datoE<-datoEME[,-c(1,6,7,8)]
datoE
# Asignar nombres a los renglones
rownames(datoE)
nombres<- datoEME$Nombre
faces(datoE, label=nombres)
stars(datoE)
attach(datoE)
plot(Edad, datoE$Cuanto.dinero.traes.en.este.momento)
pairs(datoE)
install.packages("andrews")
library(andrews)
andrews(datoE)
#Perfiles
library (MASS)
parcoord(datoE)
library(lattice)
splom(~datoE)

#Perfiles
install.packages("MASS")
library (MASS)
parcoord(datoE)
llbrary(lattice)
splom(~datoE)

#Clase 03/12/2024
#Matriz de datos
dim(A)
n=300
p=5
#Vector de medidas
m<-matrix(,1,p)
s<-matrix(,1,p)

for(i in 1:p){
  m[1,i]<- mean (A[,i])
  s[1,i]<- sd (A[,i])
}

z<-matrix(,n,p)
for (i in 1:n){
  for(j in 1:n){
    z[i,j] <- (A[i,j]<-m[1,j])/s[1,j]
  }
}

#######Clase 09/12/2024

install.packages("psych")
library(psych)
install.packages("psy")
library(psy)
install.packages("polycor")
library(polycor)
install.packages("ggcorrplot")
library(ggcorrplot)
datoF <- read.csv("InsegUV1.csv", header = T)
datoF
datoF1<-datoF[,1:20] 
aa<-cor(datoF1)
aa
det(aa)
#Las variables si estan correlacionadas
cortest.bartlett(aa)
# ¿Los datos son adecuados para el análisis factorial?
KMO(aa)
# Ejecutar el análisis factorial con 5 factores
model<-factanal(datoF1, factors = 5, method ="mle")#varimax
model
# Calcular el número de factores ideal
scree.plot(datoF1,type = 'R')

library(stats)
acp<-prcomp(datoF1,scale = TRUE)
summary(acp)
install.packages("factoextra")
library(factoextra)
fviz_screeplot(acp, addlabels = TRUE, ylim = c(0, 20))

#### alfa
install.packages("umx")
library(umx)
install.packages("psychometric")
library(psychometric)
s<-var(datoF1)
s
reliability(s)

####Clase 10/12/2024####
#Correlacion canonica
install.packages("fields")
library(fields)
install.packages("GGally")
library(GGally)
install.packages("CCA")
library(CCA)
install.packages("CCP")
library(CCP)

hatAbril <- read.csv("HATCO2.csv", header = TRUE)
Hat <- hatAbril[,-1]
head(Hat)
GX <- Hat [,c(2, 4, 6, 7)]
GY <- Hat [,c(1, 3, 5)]
head(GX)
head(GY)
HATT <- cbind(GX, GY)
head(HATT)
ggpairs (GX, title = "Empresa")
ggpairs (GY, title = "Servicios")
cor(Hat)
ggduo (Hat, columnsX = 1:4, columnsY = 5:7, types = list(continuous = "smooth_lm"),
       title = "Correlacion entre variables HATCO",)
M <- matcor(GX, GY)
M
CC <- cc(GX, GY)
CC
rho <- CC$cor
n <- dim(HATT)
p <- length(GX)
q <- length(GY)
p.asym (rho, n, p, q, tstat = "Wilks")
plt.cc(CC, var.label = TRUE, dl = 1, d2= 2, type = "b")
img.matcor(M, type = 2)
barplot(CC$cor, xlab = "Dimension", ylab = "Correlaciones canonicas",
        names.arg = 1:3, ylim = c(0,1))
plt.cc(CC)


####Escalamiento multidimensional 7/01/25####

install.packages("smacof")
library (smacof) 
file.choose()
dista <- read.csv("/Users/marieladominguezdominguez/Desktop/kilom.csv", header = T)
dista
ciu<-dista[,1]
dim(ciu)<-c(6,1)
D<-dista[1:6,2:7]
D<-as.matrix(D)
I<-diag(6)
uno<-rep(1,6)
dim(uno)<-c(6,1)
uno
unot<-t(uno)
unot
h<-(1/6)*uno%*%unot
P<- I-h
Q<-(-.5)*P%*%D%*%P
Q1<-Q/100
prop<-eigen(Q1)
prop
prop$values[1:5]
m<-100*(sum(prop$values[1:2])/sum(prop$values[1:5]))
m
v1<-prop$vectors[,1]
dim(v1)<-c(6,1)
V1<-v1*sqrt(prop$values[1])
v2<-prop$vectors[,2]
dim(v2)<-c(6,1)
V2<-v2*sqrt(prop$values[2])
ciudad <- cbind(V1, V2)
rownames(ciudad) <- c("M","B","V","S", "SS","C")
#plot(ciudad, xlab="Componente 1", ylab="Componente 2", main="Representación de Ciudades")
#text(ciudad, labels=rownames(ciudad), pos=4, col="blue")
plot(ciudad) 

#Código  2
library(ggplot2)               
state.x77
cor(state.x77)
dim(state.x77)
head(state.x77,6)
#Distancias
state_dist<-dist(state.x77,method = "euclidian") 
#Dimensiones
cmdscale(state_dist)->cmd_state    
#Las dimensiones como data.frame
cmd_state<-as.data.frame(cmd_state)
#Gráfico
ggplot(cmd_state,aes(x=V1,y=V2,label=rownames(state.x77)))+
  geom_text(alpha=0.8,size=3,col="salmon")

print(UScitiesD)
cmdscale(UScitiesD)->mds_cities
as.data.frame(mds_cities)->mds_cities_df
ggplot(mds_cities_df,
       aes(x=V1,y=V2,
           label=rownames(mds_cities)))+
  geom_text(alpha=0.7,size=3,col="steelblue")

mds_cities<- mds_cities%*%matrix(c(-1,0,0,-1),2,2)
mds_cities_df <-as.data.frame(mds_cities)
ggplot(mds_cities_df,
       aes(x=V1,y=V2,
           label=rownames(mds_cities)))+
  geom_text(alpha=0.7,size=3,col="red")

#Representar a 3D
cars.dist <- dist(scale(mtcars))
cmds3 <- data.frame(cmdscale(cars.dist, k = 3))

install.packages("scatterplot3d",dependencies=T) 
library(scatterplot3d)

scatterplot3d(cmds3, type = "h", pch = 19, lty.hplot = 2)

##Comparando métodos

file.choose()
datoF <- read.csv("/Users/marieladominguezdominguez/Desktop/pais.csv", header = T)
datoF
datoF[,1]
dimnames(datoF)[[1]] <- c("Algeria", "Cameroon", "Madagascar", "Mauritius", "Reunion", "Seychelles", "South Africa(C)", "South Africa(W)", "Tunisia","Canada", "Costa Rica", "Dominican Rep", "El Salvador", "Greenland", "Grenada", "Guatemala", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Trinidad(62)", "Trinidad (67)", "United States (66)", "United States (NW66)", "United States (W66)", "United States (67)", "Argentina", "Chile", "Columbia", "Ecuador")

datoF1<-datoF[,-1]
datoF1
distan<-dist(datoF1,diag = TRUE, upper = TRUE)
distan
modelo <- cmdscale(distan)
modelo
distancia <- dist(modelo)
distancia
plot(modelo, type = "n", xlab = "Coord. 1", ylab = "Coord. 2")
text(modelo[, 1], modelo[, 2], labels = rownames(modelo), cex = 0.8)

pri<- princomp(datoF1, cor=T)
summary(pri)
loadings(pri)
biplot(pri)

datoF1
# PCA
pca <- prcomp(iris[,-5], scale=TRUE)
df.pca <- pca$x
# Cluster 
kc <- kmeans(datoF1,3)

library(ggplot2)
library(car)
library(dplyr)
library(factoextra)
library(cluster)
library(simstudy)
library(data.table)
library(tidyverse)
library(PerformanceAnalytics)
library(corrr)

d2f=data.frame(datoF1)
km_clusters <- kmeans(x = d2f, centers = 3, nstart = 50)

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = d2f, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
             pointsize=0.5,outlier.color="darkred") +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +  theme(legend.position = "none")

#Codigo 3
library (smacof) 
data (kinshipdelta) 
datos <- kinshipdelta  
datos
#tia, hermano, primo , hija, padre, nieta, abuelo,abuela, nieto, madre,
#sobrino, sobrina, hermana, hijo, tío 

simetricos <- smacofSym(datos, ndim=2, type="interval")
simetricos 
summary(simetricos) 
plot (simetricos, main="Solución simple con smacof", cex.main=3) 
plot (simetricos, plot.type="Shepard", main="Figura 5.2:Diagrama Shepard", cex.main=5,cex=2)
plot (simetricos, plot.type="resplot", asp=1, main="Figura 5.3: Grafico de residuos. MDS metrico", cex.main=1) 
library (smacof) 
no_metrica <- smacofSym(trading, ndim=3, type="ordinal",ties="secondary")  
no_metrica 
summary (no_metrica)

plot (no_metrica, main="Solucion no metrica en tres dimensiones", cex.main=1) 
plot (no_metrica, plot.type="Shepard",main="Diagrama Shepard", cex.main=1,cex=2)
plot (no_metrica, plot.type="resplot",main="Grafico de residuos. MDS no metrico.", cex.main=1) 

res <- mds(kinshipdelta, type = "interval")
res
summary(res)
plot(res)
plot(res, type = "p", label.conf = list(label = TRUE, col = "darkgray"), pch = 25, col = "red")

set.seed(123)
res <- mds(kinshipdelta, init = "random")
res
plot(res)
plot(res,"confplot")
plot(res,"resplot")
plot(res,"Shepard")
plot(res,"stressplot")
plot(res,"bubbleplot")

#Tipos de graficos: "confplot", "resplot" 
#"Shepard", "stressplot", "bubbleplot" "histogram" (see details)

data(trading)
res <- mds(trading, ndim = 3, type = "ordinal", ties = "secondary")
res
plot(res)
## spline MDS 
delta <- sim2diss(cor(PVQ40agg))
res <- mds(delta, type = "mspline", spline.degree = 3, spline.intKnots = 4)
res
plot(res, "Shepard")

####Clase 08/01/2025####


