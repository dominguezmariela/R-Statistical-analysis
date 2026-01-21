#Se puede consultar cualquier entrada con slo seleccionar u presionar Run o control+enter
mean(c(1,7,9,14,16))

Cuchurrumin<-mean(c(1,7,9,14,16,72))
Cuchurrumin<-mean(c(1,7,9,14,16,97))
#Promedio de datos:
X<-mean(c(1,7,9,14,0,16,97))
X<-mean(c(1,7,9,14,NA,16,97))

class(a1)
class(a2)
class(x1)
class(x2)
class(x3)
class(f1)
class(f2)

pi
edades <- c(15, 19, 13, NA, 20)
deporte <- c(TRUE, TRUE, NA, FALSE, TRUE)
Sheroe<- c(NA, 'Superman', 'Batman', NA, 'Batman')
alumnos<- c("MIGUEL EDUARDO", "MARISOL", "DIEGO IVAN", "JESUS EMMANUEL", 
            "ITZEL", "MARIELA JUDITH", "EVERARDO", "YAIR ANTONIO", 
            "MARIA VIVIANA", "LUIS ENRIQUE", "DULCE MARIA", "JOSUE ENRIQUE", 
            "CESAR", "JULIET DALILA", "MARELY", "MELISSA", "ARLETH MICHELL", 
            "JUAN ANTONIO", "ALAN YAHIR", "ARANXA", "KARLA BELEN",
            "XIOMARA YOLOTZIN", "DOMINIQUE", "ISABEL","ÁNGEL ARGÚELLO")
alumnos[c(1,3,6)]
sheroe[c(-2)]
cuchurrumin<-NULL

x <- NULL
x1 <- c(1,2,3,4,5,6,7,8,9,10)
x2 <- c(1:10)
x3 <- seq(1,100,10)

a <- NULL
a1 <- c("a","b","c","d","e","f")
a2 <-c("Hombre","Mujer","Hombre","Hombre","Mujer","Hombre",
       "Mujer","Mujer","Hombre")

Cuchurrumin <-c("Hombre","Mujer","Hombre","Hombre","Mujer","Hombre","Mujer",
                "Mujer","Hombre")

readline()
cuhurrumina<-readline()
cuchurrumina<-as.numeric(cuchurrumina)
cuchurrumina<-as.numeric(readline(prompt = "los minutos de la hora que 
                                  transcurre"))

f2 <- factor(a2); f2
pachuco40<- c(1,2,3,1,1,2,3)
Conflictos<- c(2,2,2,2,3,2,4,2,5,4)
Conflictos_f <- factor(Conflictos)
Confictosdesc<- factor(Conflictos, levels=c(1:5), labels("CD02", "CD03", 
                                                         "CD04","CD05"))
f3 <- factor c(1,2,3,1,1,2,3) 
levels = c(1:3)
          labels = c("Independiente","Dependencia leve","Dependencia moderada"))
f4 <- cut(x1,breaks = c(0,3,6,8,10), labels = c("Primero","Segundo","Tercero",
                                                "Cuarto"))
casoscancer<-c(4,0,0,3,3,5,1,3,4,0,1,1,1,0,4,2,1,3,0,4,3,0,3,3,4,4,3,1,5,1)
levels=c(0:5)
labels("muy precoz","precoz","temprano","intermedio","avanzado","terminal")
etapa<-factor(casoscancer, levels = (0:5))
              labels = ("muy precoz","precoz","temprano","intermedio",
                        "avanzado","terminal")
nombre <- readline(prompt="Nombre: ")
edad <-as.numeric(readline(prompt="Edad; "))
estatura <- as.numeric (readline(prompt="Estatura: "))
peso <- as.numeric (readline(prompt="Peso: "))
nchar(nombre)
imc <- (peso/estatura^2)

paste(nombre, "tiene ",edad, "años y pesa", peso, "kilos, midiendo",estatura,
      "metros, con un IMC de", imc)
resultadof <- c("Aprobó", "Aprobó", "Reprobó", "Reprobó",
                 "Reprobó", "Reprobó", "Reprobó", "Reprobó", "Aprobó",
                 "Reprobó", "Aprobó", "Reprobó", "Reprobó", "Aprobó",
                 "Aprobó", "Reprobó", "Aprobó", "Reprobó", "Aprobó","Aprobó",
                "Aprobó", "Reprobó", "Aprobó", "Reprobó")
paste(alumnos,Sheroe,sep=" - ")
paste(alumnos, collapse = " & ")
secuencia<-paste(alumnos, collapse = " & ")
mmatriz <- matrix(data=1:20, nrow=4, ncol=5, byrow=FALSE)
mmatriz1 <- matrix(data=1:20, nrow=4, ncol=5, byrow=TRUE)
mcasoscancer<-matrix(casoscancer, nrow = 30,ncol = 10, byrow = FALSE)

dim(mcasoscancer)<-c(30.2)
marco <- data.frame(edades, deporte, Sheroe)
marco$edades
marco2<-data.frame(alumnos, resultadof)
marco2$alumnos
sheroealumnos<-paste(marco2$alumnos,marco2$resultadof,sep="¿Qué creen?")

subset(marco, subset=deporte==TRUE)
subset(marco, subset=edades>=17)
subset(marco, subset=edades>=17, select=c("edades", "deporte"))
subset(marco2, subset = resultadof=="Aprobó")
seleccionados<-subset(marco2, subset = resultadof=="Aprobó")

gorditos<-subset(eme2024, subset=(eme2024$peso/eme2024$estatura^2)>=30)

install.packages("haven")
installed.packages("foreign")
library(haven)

enaho17_m1_A <- as.data.frame(read_sav("Enaho01-2017-100.sav"))
Osos<- data.frame(x1,x3,f4)

#Para abrir el buscador de archivos y seleccionar uno
file.choose()

data <- read.table("/Users/marieladominguezdominguez/Desktop/20240902_PE.txt", header = TRUE)
data()

arreglo <- array(data=letters[1:24], dim=c(3, 4, 2))
arreglo1 <- array(data=letters[1:24], dim=c(6, 2, 3))

letras<-c(letters[1:24])
arregloalumnos <-array(alumnos[1:24], dim =c (3,4,2))

  sample(alumnos, 8, replace= FALSE)
purodiez<-sample(alumnos, 8, replace= FALSE) 
arreglo3<- array(purodiez[1:8], dim=c (2,2,2))

sample(alumnos, 24, replace= FALSE)
arreglo4<-array (sample(alumnos, 24, replace = FALSE)), dim=c(3,4,2)
mimatriz <- matrix(data=1:20, nrow=4, ncol=5, byrow=FALSE)

lista1 <- list(L1=alumnos, L2=mimatriz, L3=arreglo)

#Consulta los paquetes instalados
installed.packages()
# Para ver un resumen de lo realizado en sa sesión
ls()

file.choose()
read.table(file.choose(), header=TRUE, sep="\t", dec=".")
read.table("ProgramaciónEjercicio.xlsx", header=TRUE, sep="\t", dec=".")

install.packages("eulerr")
library(eulerr)
set.seed(1)
s2 <- c(A = 1, B = 2)
plot(venn(s2))
plot(euler(s2), quantities = TRUE)
borar <- fruits
plot(venn(fruits[, 1:3]))
library(dplyr)
plot(euler(fruits[, 1:3], shape = "ellipse"),
     quantities = TRUE)

install.packages("dplyr")
library(dplyr)


A <- c(1:21)
B <- c(13:33)
# U=universo
U <- c(34:44)

#unión de AB
conjuntol <- unique(c(A, B))
AUB <- union(A, B)
AyB <- intersect(A,B)

#Elementos de AUB/tamaño
length(AUB)

AUB<-union(A, B)
universo<- union(union(A,B), U)

BmenosA <- setdiff(B,A)

#Complemento en conjuntos
Acomplemento <- setdiff(U, B)
Bcomplemento <- setdiff(U, A)

AUBcomlemento<-setdiff(univres)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggVennDiagram")
library(ggVennDiagram)

# Lista de vectores
x <- list(A = 1:5, B = 2:7)


# Diagrama de Venn 2D
ggVennDiagram(x) 


# Lista de vectores
x <- list(A = 1:5, B = 2:7, C = 5:10)

# Diagrama de Venn 3D
ggVennDiagram(x) 

plot(venn(organisms))

grupo1<-sample(alumnos, size = 10)
grupo2<-sample(alumnos, size = 10)
grupo3<-sample(alumnos, size = 10)

g1yg2<-intersect(grupo1, grupo2)

resultado1 <- rep(c("Aprobatorio", "Reprobatorio", "Incierto"), 
                  times=c(19,2,3))
sample(resultado1, size = 24)

resultado1 <- sample(rep(c("Aprobatorio", "Reprobatorio", "Incierto"), 
                  times=c(19 ,2,3)))
resultadoeme<-data.frame(alumnos, resultado1)
paste("A quíen repruebo?", sample(alumnos,1))

resultados<- replications(num_veces,{sample(alumnos, 1)})   

prop.table()

Chabelo<-c("C1", "C2", "C3", "Ex", "Ex", "Ex")

table(resultadoeme$resultado1)
prop.table(table(resultadoeme$resultado1))

resulta2 <- c("Aprobatorio", "Reprobatorio", "Incierto")

resulta3 <- expand.grid(Estudiante=alumnos, Resultados=resulta2)

#Tabla de frecuencias 
table(resulta3)

numeros <- c("As", "Dos", "Tres", "Cuatro", "Cinco", "Seis", "Siete",
             "Ocho", "Nueve", "Diez", "Jack", "Reina", "Rey")
palos <- c("de Corazones", "de Diamantes", "de Picas", "de Tréboles")
combinatoria <- expand.grid(numero = numeros, palo = palos)
paste(combinatoria$numero, combinatoria$palo)

sample(paste(combinatoria$numero, combinatoria$palo), 5)

# Combinaciones
install.packages("gtools")
library(gtools)

N<-length(alumnos)
n<-2
combinalum <- combinations(N, n, v=alumnos)
combinalum1 <- combinations(N, n, alumnos)

melate <- combinations(44, 7, v=1:44)

combinalum1 <- combinations(N, n, v=alumnos, repeats=FALSE)
combinalum1 <- combinations(N, n, alumnos, repeats=TRUE)
head(combinalum)
tail(combinalum)

combinalum2 <- combinations(N, n, v=alumnos,set = FALSE, repeats=FALSE)
combinalum2 <- combinations(N, n, v=alumnos,set = TRUE, repeats=FALSE)

#n=total de elementos 
#r=número de elementos que necesitamos


combinations(5, 2, v=alumnosVector, set = TRUE, repeats= FALSE) 
alumnosVector<- sample(alumnos,  5, replace=FALSE)
sample()
combinations(5,2, alumnosVector)

permuntations(5, 2, v=alumnosVector, set = TRUE, repeats= FALSE) 

#p= probabilidad de éxito
#q= probabilidd de fracaso


#d: función de densidad o de probabilidad.
#p: función de distribución
#q: función para el cálculo de cuantiles.
#r: función para simular datos con dicha distribución.


p <- 0.7
vbernoulli <- rbinom(10, 50, p)
mean(vbernoulli)

#con las funciones de la tabla de equivalencias (diapo 131)
runif(24, min=0, max=1)

#Semilla
set.seed(13245)
p <- 0.79
rbinom(1, 5, p)
p <- 0.6
rbinom(50, 10, p)
vbinom<-rbinom(pruebas,cuantos,p)
meanbinom<-mean(vbinom)
probabinom<-meanbinom/cuantos

#la probabilidad es acumulable (hasta llegar a 1), como se ve en los resultados del ensayo a continucación:
dbinom(0:10, 5, p) #=[1] 0.0004084101 0.0076819995 0.0577979010 0.2174301990 0.4089758505
#[6] 0.3077056399 0.0000000000 0.0000000000 0.0000000000 0.0000000000
#[11] 0.0000000000

dbinom(0:1000, 5, p)

#Abrir una llave nunca debería ocurrir en su propia linea y siempre se sigue 
#con una linea nueva. Una llave que cierra siempre debe ir en su propia linea 
#a menos que sea -else-.

#Funciones definidas por el usuario
base=10
altura=25
area_t(base, altura)
area_t <- function(base, altura) {
  (base*altura)/2
  } 

valores<-rbinom(100, 500, 0.63)
mediadevalores<-function(datos){
  mean(datos) length(datos) 
  paste("El promedio de datos fue:",mean(datos))
  } 

califica<-runif(24,8,10)
resultadoestudiantes <- function (seleccionado,calif) {
  seleccionado <- sample (alumnos, size = 1) 
  calif<-sample (califica, size = 1)
  paste (seleccionado," obtuvo una calificacion de ", calif)
  } 

resultadoestudiantes (alumnos, califica)

#xlab/ylab= para dar nombre a los ejes x/y del histograma
#datos=variable. nombre=texto cualquiera/encabezado del histograma, este se define al final en crear_histograma

crear_histograma <- function (datos, nombre) {
  media <- mean (datos)
  desv_est <- sd (datos)
  hist(datos, main = nombre, xlab = "Datos", ylab = "Frecuencia", col =
         "green")
  abline (v = media, col = "red")
  abline (v = media + (desv_est * c(1, -1)), col = "blue")
}

ingreso <- rnorm(1500, mean = 15000, sd = 4500)
ingreso[1:10]
crear_histograma(ingreso, "Ingreso")

edad <- c(22,39,28,28,31,22,25,48,24,24,24,25,28,29,28,27,27,31,22,22,22,23,52)
crear_histograma (edades, "edades")

#round= digit= para redondear/pedir decimales

round(sample (60:100, 5) / 10, digit = 0)

#sort=para ordenar de forma ascendente o descendente los datos
sort(edad, decreasing = FALSE)

#if, lo que se encuentra entre comillas es la operación/resultado que arrojará si se cumple la condición (si es que la condición está especficada, si no lo está no colocará nada)

if (4 > 3) {
  "Verdadero"
}

if (4 > 3) {
  "Verdadero"
} else {
  Falso
}

#60:100, 5= una muestra de valores ente 60 y 100, de tamaño 5

#operación larga:
dato <- c (round (sample (60 : 100, 5) / 10, digits = 0))
mean(dato)
#operación corta:
promedio<- mean (c (round (sample (60 : 100, 5) / 10, digits = 0)))

if (promedio<=7) {
  print ("Aprobado con bajo promedio")
} else if (promedio <= 9){
  print("Aprobajo con promedio Alto")
} else {
  print("Aprobado con promedio Muy alto")
}
 #Ejemplo igual al anterior, pero agregando paste
if (promedio<=7) {
  print (paste ("el cálculo fue de:" ,promedio, "y el resultado fue : aprobado con bajo promedio"))
} else if (promedio <= 9) {
  print (paste ("el cálculo fue de:" ,promedio, "y el resultado fue : aprobado con promedio alto"))
} else {
  print (paste ("el cálculo fue de:" ,promedio, "y el resultado fue : aprobado con promedio muy alto"))
}

#Ejemplo igual al anterior pero con guitarras xD 
if(guitarra=="Afinada"){
  print("Comienza el concierto, arrancamos")
} else{
  print("Inicia afinando la cuerda 5 y continua con la 3")
  cuerda3 <- sample(c("Afinada", "Sin afinar"),1)
  if(cuerda3 == "Afinada"){
    print("Afina 1 con 3 e inicia el concierto")
    cuerda1 <- sample(c("Afinada", "Sin afinar"),1)
  }else {
    print("sigue trabajando con la cuerda 3")
  }
}

#ejemplo anterior pero redactado de forma diferente

guitarra <- sample(c("afinada", "sin afinar"),1)
cuerda1 <- sample(c("afinada", "sin afinar"),1)
cuerda2 <- sample(c("afinada", "sin afinar"),1)
cuerda3 <- sample(c("afinada", "sin afinar"),1)
cuerda4 <- sample(c("afinada", "sin afinar"),1)
cuerda5 <- sample(c("afinada", "sin afinar"),1)
cuerda6 <- sample(c("afinada", "sin afinar"),1)  

if(guitarra=="afinada") {
  print("Comienza el concierto, arrancamos")
} else{
  print("afina la cuerda 5 y continua con las demas")
  if (cuerda5=="afinada") {
    print ("continua con la cuerda 3")}
  else {print ("repite hasta que esté afinada")}
  if (cuerda3=="afinada") {
    print ("continua con la cuerda 1")}
  else {print("repite hasta que esté afinada")}
  if (cuerda1=="afinada") {
    print ("continua con la cuerda 2")}
  else {print("repite hasta que esté afinada")}
  if (cuerda2=="afinada") {
    print ("continua con la cuerda 4")}
  else {print ("repite hasta que esté afinada")}
  if (cuerda4=="afinada") {
    print ("continua con la cuerda 6")}
  else {print("repite hasta que esté afinada")}
  if (cuerda6=="afinada") {
    print ("Comienza a tocar")}
  else {print("tienes problemas")}  
}

#for, se utiliza para generar un bucle

for (cara in 1:6) {
  print (cara ^2)
}

dado <- c(1:6) for (cara in dado) {
  print (cara^2)
}

for (numero in 1:10) {
  print (paste ("el dato fue", numero))
}

for(integrantes in alumnos){
  print(integrantes)
  print(resultadoestudiantes(c(round(sample(60:100,5)/10,digit=0))))
  print(paste(integrantes," con promedio ",
              resultadoestudiantes(c(round(sample(60:100,5)/10,digit=0)))))
}

resultadoestudiantes(c (round(sample(60:100,5)/10, digit=0)))

cuantos <- sample (10:100,1)
for (casos1 in 1:cuantos) {
  print (paste("el resultado", casos1, "fue reprobatorio con:", 
               round(casos1/10, digits = 1)))
}

#prefijo fijo
textoraiz <- "variable_"

#bucle for para crear objetos con nombres dinámicos y asignarles el cuadrado de numero de objeto

for (i in 1:5) {
#conforma el nombre del objeto
  nombrevariable <- paste (textoraiz, i)
#crea el objeto con el nombre dinámicoy le asigna un valor (i^2 en este caso)
  assign (nombrevariable, i^2)
}

install.packages ("randomNames")
library(randomNames)

nombres <- randomNames(5,which.names="first")

#para llamar la función "combinations" se debe llamar antes la paqueteria "gtools"  después se corre el codigo
solicitantes <- c("m1", "m2", "h1", "h2", "h3")
library(gtools)
combinaciones <-combinations(5,2, solicitantes)
n <- nrow(combinaciones)

#[1,1] = [#=registro,#=variable]

cero <- 0
for (r in 1:n) {
  if((substr(combinaciones[r,1], 1,1) == "h" & substr(combinaciones[r,2], 1,1) == "h")) {
    cero <- cero + 1
  }
}

una <- 0
for (r in 1:n) {
  if((substr(combinaciones [r,1], 1,1) == "h" & substr(combinaciones[r,2], 1,1) == "m") |
     (substr (combinaciones[r,1], 1,1) == "m" & substr(combinaciones[r,2], 1,1) == "h")) {
    una <- una + 1
  }
}

dos <- 0
for (r in 1:n) {
  if((substr(combinaciones [r,1], 1,1) == "m" & substr (combinaciones[r,2], 1,1) == "m") ) {
    dos <- dos + 1
  }
}

#x= la probabilidad de que salgan seleccionadas mujeres de un total de 5 participantes
#main= titulo del grafico

x <- c(0,1,2)
proba_x <- c(cero/n , una/n, dos/n)
tabla <- data.frame(x, proba_x)
colnames(tabla) <- c("x", "Pobabilidad de X")
knitr::kable(tabla)
barplot(height = proba_x, names.arg = x, xlab="Seleccionar mujeres",
        ylab="Probabilidad", main="Histograma", col="pink4")

#ejemplo: analisis de puestos
x <- c(0,1,2)
proba_x <- c(ninguna/n , almenosuna /n, todas/n)
tabla <- data.frame (x, proba_x)
colnames (tabla) <- c ("x", "Pobabilidad de X")
knitr::kable (tabla)
barplot (height = proba_x, names.arg = x, xlab = "Mujeres seleccionadas",
        ylab = "Casos registrados", main = "Análisis de puestos", 
        col = "lightskyblue2")

#agregando con la funcion paste la fecha de hoy
barplot(height = proba_x, names.arg = x,
        xlab="Seleccionar mujeres", ylab="Probabilidad",
        main=paste("Histograma",date()), col="moccasin")

#ejemplo: edades de los alumnos
table (edad)
tablaedades <- table (edad)
print (tablaedades)
barplot (height = tablaedades, names.arg = names (tablaedades), xlab = "Edades alumnos",
        ylab = "Frecuencia", main = "Chicos de la EME", col = "mistyrose1")



#distribuciones de probabilidad

llaves <- c ("a0", "n1", "n2", "n3")
eventos <- permutations (4, 4, llaves)
nrow (eventos)
casos <- 4
prob <- NULL

for (intentos in 1:casos){
  prob <- c (prob,intentos/casos)
  prob
}

barplot (prob)

barplot (height = proba_x, names.arg = x,
        xlab = "Seleccionar mujeres", ylab = "Probabilidad",
        main=paste ("Histograma",date()), col = "blue")

#estructuras de control en R (WHILE)
#while(condicion) { operaciones }

lug_disp <- 10
clientes <- 0
espera <- 0
while(clientes < lug_disp) {
  print(paste("Lugares disponibles: ", lug_disp-clientes))
  clientes <- clientes + 1
  espera <- espera + 1
}

#Estructuras de control en R 
#BREAK indica que detención o interrupción del proceso. En el caso de un FOR:

inicio <- sample(1:10,1)
inicio
for(i in 1:10) {
  if(i == inicio) {
    break
  }
  print(i)
}

#En el caso de un WHILE

solic_eme <- 40
while(solic_eme > 5) {
  if(solic_eme == 16) {
    break
  }
  solic_eme <- solic_eme - 1
}

#La instrucción NEXT indica que detención o interrupción del proceso

caso <- sample(1:10,1)
for(i in 1:10) {
  if(i == caso) {
    next
  }
  print(i)
}

#El modelo REPEAT es:
  repeat {
    operaciones
    un_break_para_detener
  }

alumnos
cuenta <- 0
inicial <- sample(alumnos,1)
repeat {
  sorteo <- sample(alumnos,1)
  print(paste("Participó ",sorteo))
  cuenta <- cuenta + 1
  if (inicial==sorteo) {
    print(cuenta)
    break
  }
}

###distribucion poisson

registros <- 0
r<-2
eventos <- for(casos in 0:r){
  registros <- dpois(c(casos), 0.5)+registros
}

r<-3
ppois(c(r), 0.5)
registros <- 0
#r<-2
eventos <- for(casos in 0:r){
  registros <- dpois(c(casos),
                     0.5)+registros
}

registros <- 0
r<-5
eventos <- for(casos in 0:r){
  registros <- dpois(c(casos), 0.5)+registros
}
1-registros

###distribución Geométrica (o de fracasos)

p = 0.20
n = 5
dgeom(x = n, prob = p)

p = 0.20
mean(rgeom(n = 10000, prob = p)==5)

###Distribución Binomial Negativa

seleccion <- 5
r <- 3
proba <- 0.95
dnbinom(seleccion-r,r,proba)

seleccion <- 5
r <- 3
proba <- 0.95
pnbinom(seleccion-r,r,proba, lower.tail=T)

###Distribución Hipergeométrica

x<-1 # Variable a determinar su probabilidad
m <- e # Casos exitosos
k <- n # La muestra
f <- N-m # Fracasos
dhyper(x = x, m = m, k = k, n = f)

#Trasladado a: dhyper(x, m, n, k, log = FALSE)
x<-1 # Variable a determinar su probabilidad
m <- r # Casos exitosos
k <- n # La muestra
n <- N-m # Fracasos
dhyper(x = 0:k, m = m, k = k, n = n)

###Distribución Uniforme
#Estimar los resultados de la función de densidad de valores 100 valores 
#uniformes en un intervalo de 10 a 50 (a, b)

dunif (0:100, min = 10 , max = 50 )

#Cambiando los valores del intervalo y generando un valor en específico

dunif (10, min = 10 , max = 50)
dunif (10, min = 1 , max = 50)

#Cambiando los valores del intervalo y generando un valor en específico

punif (0:100, min = 10 , max = 50)

###Distribución Normal

li <- -5
ls <- 5
incr <- 0.025
v_norm <- seq (li , ls, by= incr)
y_dnorm <- dnorm (v_norm)

set.seed(12345)
muestra1 <- rnorm(1000)
hist(muestra1, main="Histrograma", ylab = "frecuencia", col="red",
     freq = F)
curve(dnorm, lty=2, add=T, col="blue")
plot(muestra1, pnorm(muestra1, mean(muestra1), sd(muestra1)),
     lty = 1, ylab="Acumulada", col="blue")
set.seed(67890)
muestra2 <- rnorm(1000)
hist(muestra2, main="Histrograma", ylab = "frecuencia",
     col="yellow", freq = F, add=T)
curve(dnorm, lty=2, xlab="Acumulada", col="green", add=T)
plot(muestra2, pnorm(muestra2, mean(muestra2), sd(muestra2)),
     lty = 2, ylab="Acumulada", col="red")

curve(ecdf(muestra1)(x), xlim = c(-3, 3), ylab = "F(x)", type = "s",
      col="red", lwd=2)
curve(pnorm(x, mean(muestra2), sd(muestra2)), lty = 1, add = TRUE,
      col="blue", lwd=2)

y_pnorm <- pnorm(v_norm )

set.seed(12345)
N <- 1000
va_norm <- rnorm(N)

###Distribución t-Student

li <- -10
ls <- 10
incr <- 0.01
n <- 4
gl <- n-1
v_stu <- seq (li , ls, by= incr)
y_dt <- dt (v_stu, df= gl)

y_pt <- pt (v_stu, df = gl)

li <- -10
ls <- 10
incr <- 0.01
n <- 4
gl <- n-1
v_stu <- seq (li , ls, by= incr)
y_dt <- dt (v_stu, df= gl)

###Distribución Ji Cuadrada

li <- 0
ls <- 20
incr <- 0.1
m<-5
v_chisq <- seq(li, ls, by=0.1)
y_dchisq <- dchisq(v_chisq, df=m)
plot(y_dchisq)+lines(y_dchisq, col="blue")

li <- 0
ls <- 20
incr <- 0.1
m<-5
v_chisq <- 3.0
y_dchisq <- dchisq (v_chisq, df=m)


#clasificación de valores en una variable numérica de un data frame
#utilizando la función cut(), que  permite crear categorías a partir
#de una variable numérica

# Crear un data frame con dos variables
df <- data.frame(Nombre = c("A", "B", "C", "D", "E"),                 
                 Edad = c(23, 45, 12, 36, 28))
#data frame
print(df)

#Clasificación de la variable Edad en rangos
#cut: para asignar categorias
df$RangoEdad <- cut(df$Edad, breaks = c(0, 18, 30, 50), 
                    labels = c("Joven", "Adulto Joven", "Adulto"), right = FALSE)
# Ver el data frame con la nueva columna categórica
print(df)

edad <- c(22,39,28,28,31,22,25,48,24,24,24,25,28,29,28,27,27,31,22,22,22,23,36,65,52)
edad1 <- c(22,NA,39,28,28,31,22,25,48,24,24,24,25,28,29,28,27,27,31,22,22,22,23,36,65,52)
bdeme <- data.frame(EME = alumnos, EdadEME = edad)
bdeme$grupo <- cut(bdeme$EdadEME, breaks = c(0, 25, 40, 59, 70),
                   labels = c("joven", "adulto joven","adulto en plenitud", "adulto mayor"))

#si tenemos un NA podemos utilizar la funcion na.rm para darle un valor logico
mean(edad1, na.rm = TRUE) 
#sum=suma
sum(edad1, na.rm = TRUE)

install.packages("readxl")
library(readxl)

datos <- read_xlsx("/Users/marieladominguezdominguez/Downloads/Sismos8521_Karla.xlsx")
View(datos)
str(datos)

install.packages("rlang")
library(rlang)
install.packages("stringr")
library(stringr)
datos$Hora <- format(as.POSIXct(as.numeric(datos$Hora),
                                origin = "2018-01-01",
                                tz = "GMT"),
                     format = "%H:%M:%S")


# Fecha y hora en formato texto
fecha_texto <- "2024-10-09 12:30:00"
# Convertir a objeto POSIXct
fecha_posix <- as.POSIXct(fecha_texto)
# Cambiar el formato de la fecha
fecha_formateada <- format(fecha_posix, format="%d/%m/%Y %H:%M:%S")
print(fecha_formateada)

#Explicación de cada parte: as.POSIXct(fecha_texto): Convierte la cadena "2024-10-09 12:30:00" en un objeto de clase POSIXct, que R puede interpretar como fecha y hora.

#format(fecha_posix, format="%d/%m/%Y %H:%M:%S"):
#format="%d/%m/%Y %H:%M:%S": Especifica el formato de la fecha.
#%d: Día del mes con dos dígitos (09).
#%m: Mes con dos dígitos (10).
#%Y: Año completo con cuatro dígitos (2024).
#%H: Hora en formato de 24 horas (12).
#%M: Minutos con dos dígitos (30).
#%S: Segundos con dos dígitos (00).


borrar <- format (as.POSIXct(datos$hora), format = "%Y")

datos <- read.table("/Users/marieladominguezdominguez/Downloads/Sismos8521_Karla.xlsx", header = TRUE)

fecha_formateada1 <- format(fecha_posix, format="%w")
fecha_formateada2 <- format(fecha_posix, format="%A")

datos$año <- as.numeric(format(as.POSIXct(datos$Hora), format= "%Y"))
datos$mes <- as.numeric(format(as.POSIXct(datos$Hora), format= "%m"))
datos$dia <- as.numeric(format(as.POSIXct(datos$Hora), format= "%d"))
datos$dma <- as.numeric(format(as.POSIXct(datos$Hora), format= "%Y-%m-%d"))
head(datos$Fecha)
head(datos$dma)

datos$Magnitud <- stringr::str_replace_all(datos$Magnitud, "no calculable", "")
magni_sna <- na.exclude(datos$Magnitud)
profu_sna <- na.exclude(datos$Profundidad)
table(is.na(datos$Magnitud))
table(is.na(datos$Profundidad))

#media aritmetica

maritmetica <- mean(datos$Magnitud, na.rm = TRUE)
paritmetica <- mean(datos$Profundidad, na.rm = TRUE)
Aaritmetica <- mean(datos$año, na.rm = TRUE)

summary(datos$Magnitud)
summary(datos$Profundidad)

datos$Magnitud <- stringr::str_replace_all(datos$Magnitud, "no calculable",
                                           "cuchurrumin")

#Procedimiento 1
datos$Magnitud <- stringr::str_replace_all(datos$Magnitud, "no calculable","")
#Procedimiento 2
datos$Magnitud[datos$Magnitud=="cuchurrumin"]<-""
#Convertimos Magnitud a variable numérica
datos$Magnitud<-as.numeric(datos$Magnitud)

boxplot(datos$Magnitud)
boxplot (datos$Magnitud, horizontal= T)

var(datos$Magnitud, na.rm = TRUE)
sd(datos$Magnitud, na.rm = TRUE)

#Exclusión de casos NA para Magnitud y Profundidad
magni_sna <- na.exclude(datos$Magnitud)
profu_sna <- na.exclude(datos$Profundidad)
table(is.na(datos$Magnitud))
table(is.na(datos$Profundidad))

#Mediana
median(datos$Magnitud, na.rm=TRUE)
table(na.exclude(datos$Magnitud))
summary(datos$Magnitud)
summary(datos$Profundidad)

#Moda
modda <- function(x) {
  return(as.numeric(names(which.max (table(na.exclude(x))))))
}
mmoda <- modda(datos$Magnitud)

#Máximo y Mínimo
max(datos$Magnitud, na.rm=TRUE)
min(datos$Magnitud, na.rm=TRUE)
#la funcion "table" nos da las frecuencias

#Media Aritmética
maritmetica <- mean(caso2$POBTOT)

#Media Geométrica
mgeometrica<-function(x) exp(sum(log(x))/n)
mgeometrica(caso2$POBTOT)

#Media Armónica
marmonica <- 1/mean(1/caso2$POBTOT)

#Mediana
median(caso2$POBTOT)

#Máximo y Mínimo
max(caso2$POBTOT)
min(caso2$POBTOT)

table(na.exclude(datos$Magnitud))

#Which.max para ver el valor que mas se repite dentro de los datos
which.max(table(na.exclude(datos$Magnitud)))

modda<- function(x) {
  return(as. numeric())
} 

#piramide
install.packages("readxl")
getwd()
file.choose()
library(readxl)
#setwd("C:\.../BD")

chihuahua <- read_excel("/Users/marieladominguezdominguez/Desktop/Posgrado/EME/ProgramaciónEstadística/20231007Chihuahua.xlsx"
                        , sheet='Tabulado', col_name=FALSE, range="Tabulado!b6:e26")
chihuahua <- read_excel(file.choose(),
                        sheet='Tabulado',
                        col_name=FALSE,
                        range="Tabulado!b6:e26")
as.data.frame(chihuahua)
str(chihuahua)
chihuahua$ ...1

install.packages("pyramid")
library(pyramid)
ch<-data.frame(Edad=chihuahua$ ...1, Total=chihuahua$ ...2, Hombres=chihuahua$ ...3,
               Mujeres=chihuahua$ ...4)
str(ch)

#EDE <- ch$Total
mean(ch$Total)
mean(ch$Hombres)
mean(ch$Mujeres)

#mean(EDE)
H1<-round(ch$Hombres/1000,0)
M1<-round(ch$Mujeres/1000,0)
Edad <- ch$Edad
ch1<-data.frame(H1,M1,Edad)
pyramid(ch1,Llab="Hombres",Rlab="Mujeres",Clab="Edad",
main="Población de chihuahua 2020 /n (en miles)",Lcol="red", Rcol="blue", Cgap=0.5)
par(mfrow=c(1,1))
datos <- read.table("/Users/marieladominguezdominguez//Sismos8521_Karla.xlsx", header = TRUE)

# Carga el paquete aplpack
install.packages("aplpack")
library(aplpack)
data2 <- data.frame(
  A = c(0.2, 0.8, 0.4, 0.5, 0.9),
  #A = c(runif(5,17,51)),
  B = c(rnorm(5,7.9, 4.73)),
  C = c(0.5, 0.6, 0.7, 0.4, 0.3),
  D = c(0.4, 0.3, 0.2, 0.5, 0.6),
  E = c(0.9, 0.2, 0.4, 0.8, 0.7))

# Crea el gráfico de caritas de Chernoff
faces(data2)
runif(5,17,51)

install.packages("xQuartz")
library(xQuartz)
