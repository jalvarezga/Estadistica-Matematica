getwd()
#nos da el working directory, el ambiente (parte de la compu) en el que todos los archivos estan siendo tomados y guardados
# tamanos de muestra n=10,100,1000
#generamos 10000 repeticiones para cada tamano de muestra, para cada estimador
#primero vamos con T1
#T1 es la media muestral
rnorm(n=10,mean=0,sd=1) #genera n normales indep con media 0 y desviacion estandar 1,
#una m.a. de tamano 10 de una normal estandar
n=10
for( i in 1:n)
  print(1)
#se puede no poner las llaves, pero a mi si me gusta ponerlas pues le da mas orden, claridad y estructora al codigo
observaciones1=1:10000
for(i in 1:10000){
  t1=mean(rnorm(n,mean=0,sd=1)) #una realizacion del estimador t1
  observaciones1[i]=t1 #la guardamos en la entrada i del vector observaciones1
}
hist(observaciones1)
#podemos hacer ajustes a ese plot
hist(observaciones1, xlim =c(-2,2), ylim=c(0,3000), xlab='observaciones1', ylab="frecuencia") #hace un histograma(diagrama de frecuencias de las observaciones)
plot(density(observaciones1), type='o', col='red', main='Plot de  densidad de T1 con n=10 como tam de muestra') #podemos cambiar el formato
#ok eso era para darnos ideas, ahora vamos a sistematizar mas el asunto.
n=10
observaciones11=1:10000
for (k in 1:10000){
  t1=mean(rnorm(n,mean=0,sd=1)) #promediamos las n realizaciones de la m.a. de tamano n
  observaciones11[k]=t1
}
hist(observaciones11)
density(observaciones11)
plot(density(observaciones11), main= "Densidad de T_1 para distintos tamaños de muestra", xlab='valores de T1', xlim=c(-1.5,1.5), col='green4', ylim=c(0,5))
n=100
observaciones12=1:10000
for (k in 1:10000){
  t1=mean(rnorm(n,mean=0,sd=1)) #promediamos las n realizaciones de la m.a. de tamano n
  observaciones12[k]=t1
}
lines(density(observaciones12), col='red')
n=1000
observaciones13=1:10000
for (k in 1:10000){
  t1=mean(rnorm(n,mean=0,sd=1)) #promediamos las n realizaciones de la m.a. de tamano n
  observaciones13[k]=t1
}
lines(density(observaciones13), col='blue',lwd =1, type='l') #lwd es el grosor type puede ser:l, s,b, p, o, h
legend("topleft",
c("n=10", 'n=100', 'n=1000'),
fill=c("green4","red", 'blue')
)
#
#notamos que conforme n crece la densidad se colapsa al valor de mu=0, por lo que T1 es consistente
#ademas la densidad es simetrica respecto a mu=0 por lo que T1 es insesgado 
#
#
#
#
#
#ahora vamos con T_2, la mediana muestral
#al generar una simulacion de la muestra, la ordenamos y nos quedamos con el valor que quede en medio
#por ejemplo, para n=10, queremos 0.5(x_(5)+x_(6))
n=10
v=rnorm(n)
print(v)
sort(v)
#ordena de menor a mayor
sort(v)[2] #agarra el segundo valor mas chico de v
n=10
observaciones21=1:10000
for(j in 1:10000){
  real=rnorm(n) #tomamos muestra de tamano n
  ordenada=sort(real) #la ordenamos
  t2=0.5*(ordenada[n/2]+ordenada[n/2+1]) #construimos realizacion de T2, la mediana muestral, para el caso en que n es par
  observaciones21[j]=t2 #la guardamos en la j esima entrada del vector observaciones21
}
plot(density(observaciones21), main='Densidad de T2 para distintos tamanos de muestra', col='purple',lty=3,lwd=1 ,type='l', xlim=c(-1.5,1.5), ylim=c(0,5)) #lwd es el grosor, lty le da mas formato aparte de type
n=100
observaciones22=1:10000
for(j in 1:10000){
  real=rnorm(n) #tomamos muestra de tamano n
  ordenada=sort(real) #la ordenamos
  t2=0.5*(ordenada[n/2]+ordenada[n/2+1]) #construimos realizacion de T2, la mediana muestral, para el caso en que n es par
  observaciones22[j]=t2 #la guardamos en la j esima entrada del vector observaciones21
}
lines(density(observaciones22), col='orange',type='l', lwd=2 )
n=1000
observaciones23=1:10000
for(j in 1:10000){
  real=rnorm(n) #tomamos muestra de tamano n
  ordenada=sort(real) #la ordenamos
  t2=0.5*(ordenada[n/2]+ordenada[n/2+1]) #construimos realizacion de T2, la mediana muestral, para el caso en que n es par
  observaciones23[j]=t2 #la guardamos en la j esima entrada del vector observaciones21
}
lines(density(observaciones23), col='black',type='l', lwd=1, lty=1)
?legend
legend(.4,3, fill=c('purple', 'orange', 'black'),c('n=10','n=100', "n=1000") )
#las densidades tiene caracteristicas  cualitativas similares a las densidades de T1
#ahora vamos con T3, el medio rango muestral
#
#
#
n=10
observaciones31=1:10000
for(u in 1:10000){
  muestra=rnorm(n)
  ordenada=sort(muestra)
  t3=(ordenada[1]+ordenada[n])/2
  observaciones31[u]=t3
}
plot(density(observaciones31), main='Densidades de T_3 para diferentes tamaños de muestra', col='black', type='l' ,xlim=c(-1.5,1.5), ylim=c(0,5), xlab='valores que toma T3', ylab='Densidad')
n=100
observaciones32=1:10000
for(u in 1:10000){
  muestra=rnorm(n)
  ordenada=sort(muestra)
  t3=(ordenada[1]+ordenada[n])/2
  observaciones32[u]=t3
}
lines(density(observaciones32), col='blue', type= 'l', lty=1)
n=1000
observaciones33=1:10000
for(u in 1:10000){
  muestra=rnorm(n)
  ordenada=sort(muestra)
  t3=(ordenada[1]+ordenada[n])/2
  observaciones33[u]=t3
}
lines(density(observaciones33), col='red', type= 'l', lty=2, lwd=2)
legend('topright', fill=c('black','blue', 'red'), c('n=10', "n=100", 'n=1000')) #lty=c(1,1,2))#, lwd=c(1,1,2))
?legend
#notamos que T3 es insesgado, pues las densidades tienen simetria, sin embargo conforme n crece no se concentra tanto la distribucion de muestreo alrededor del 0.



#ahora un plot comparativo para el caso n=100
plot(density(observaciones12), col='black', main='Distribución de muestreo para los distintos estimadores de mu con tamaño de muestra n=100', ylim=c(0,4))
lines(density(observaciones22), col='blue')
lines(density(observaciones32), col='green4', type='l')
legend('topright', fill=c('black', 'blue', 'green4'), c('media muestral(T1)', 'mediana muestral(T2)','medio rango muestral(T3)'))
#notamos que el mejor estimador es T1 pues se acumula mas probabilidad alrededor del cero, tiene una cola menos pesada y por lo tanto menos varianza
#el peor de estos tres estimadores es T3 pues tiene colas mas pesadas y por lo tanto tiene mas varianza como estimador puntual de mu=0
#se consttuyeron estas densidades a partir de 10mil observaciones de cada uno de los estimadores con tamaño de muestra n=100 para cada realiazcion de los estimadores

##
#
#
#vamos a estimar algunas propiedades de estos estimadores como su media y su varianza
#primero T1 es la media muestral, el cual es un estimador insesgado de mu pero podemos verificarlo con un 
#enforque frecuentista de la Ley de los grandes numeros
mean(observaciones11)
mean(observaciones12) #se acerca más al valor real
mean(observaciones13)
#
mean(observaciones21)
mean(observaciones22)
mean(observaciones23)
mean(observaciones31)
mean(observaciones32)
mean(observaciones33)

#ahora vamos a calcular varianzas usando la varianza muestral (1/(n-1))suma((xi-xbarra)^2)
#vamos a comparar varianzas para los estimadores con muestras de tamano 100
#
var1=0
media1=mean(observaciones12)
for(i in 1:10000){
  var1=var1+(media1-observaciones12[i])^2
}
var1=var1/9999
var1
var2=0
media2=mean(observaciones22)
for(i in 1:10000){
  var2=var2+(media2-observaciones22[i])^2
}
var2=var2/9999
var2
#
var3=0
media3=mean(observaciones32)
for(i in 1:10000){
  var3=var3+(media3-observaciones32[i])^2
}
var3=var3/9999
var3
var1
var2
var3
#notamos que hay coherencia entre nuestro analisis cualtitativo/visual de las distribuciones de muestreo con los resultados numericos
#los estimadores son insesgados, 
#T1 tiene menos varianza y T3 es el estimador con mayor varianza
#NICEEEE
#por lo tanto, el mejor estimador de los 3 es T1, lo cual hace sentido pues es una estadistica suficiente para mu
#y los otros estimadores no son suficientes.
