#Datos normalidad
y<-c(1.3,2.8,4.2,1.5,7.3,4.5,5.1,6.8,4.3,5.6,2.9)
h<-c(3.4,1.9,2.5,2.7,3.4,5.1,1.7,1.2,5.6,4.3,1.4)
w<-c(1.4,3.9,2.6,2.1,3.7,5.4,3.7,2.2,6.6,5.3,3.4)

pro<-matrix(,11,5)

k<-sort(y)

pro[,1]<-t(k)

h1<-sort(h)

pro[,2]<-t(h1)

w1<-sort(w)
pro[,3]<-t(w1)
n<-11
pro
for(i in 1:n)
{
pro[i,4]<- (i-0.5)/n
pro[i,5]<- qnorm(pro[i,4])
}
pro
plot(pro[,5],pro[,1])

plot(pro[,5],pro[,2])

plot(pro[,5],pro[,3])
pro


#Nomalidad multivariante
W<-pro[,1:3]
W
p<-3
n<-11
m<-matrix(,1,p)
for(j in 1:p)
{
m[1,j]<-mean(W[,j])
}
m
s<-var(W)
s
#inversa de la matriz de varianzas
s1<-solve(s)
s1
#vector para guardar las distancias
d<-matrix(,11,3)
d
#Ciclos para guardar la distancia de Mahalanobis
for(i in 1:n){
ve<-matrix(,p,1)
for(j in 1:p){
ve[j,1]<-W[i,j]-m[1,j]}
v1<-t(ve)
d[i,1]<-v1%*%s1%*%ve
}
d  #distancias
for(i in 1:n){
d[i,2]<-(i-0.5)/n
}
d
for(i in 1:n)
{
d[i,3]<-qchisq(d[i,2],(p-1))
}
d
ch<-sort(d[,1])
plot(d[,3],ch)