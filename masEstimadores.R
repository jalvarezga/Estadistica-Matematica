#estimadores de theta
#vamos a hacer simulaciones para n=10, n=100. Tomamos theta=3
getwd()
#t11=1/xbarra
#n=10
#vamos a hacer histogramas y densidades
help(rexp)
rexp(n=2, rate=3)
#vamos a hacer mil simulaciones (realizaciones) para cada estimador para cada tamaño de muestra utilizado
n=10
r_11=1:1000
for(i in 1:1000){
  sim=rexp(n,rate=3) #generamos una observacion de nuestra muestra aleatoria de tamano n
  obs=1/mean(sim) #obtenemos una observacion de nuestra estadistica (estimador de theta) tau_11
  r_11[i]<-obs
}
hist(r_11, main='Histograma para 1000 observaciones del estimador tau_11, con tamaño de muestra n=10', col='blue')
#notamos que se concentra bastante el histograma en una vecindad de 3, que es theta, el parametro que queremos estimar
plot(density(r_11), lty=1)
plot(density(r_11), lty=4, col='green', main='Densidad de la variable aleatoria tau_11 con tamano de muestra n=10')
#media
mean(r_11)
#
#
#n=100, tau_11
n=100
r_12=1:1000
for(i in 1:1000){
  sim=rexp(n,rate=3) #generamos una observacion de nuestra muestra aleatoria de tamano n
  obs=1/mean(sim) #obtenemos una observacion de nuestra estadistica (estimador de theta) tau_11
  r_12[i]<-obs
}
hist(r_12, main='Histograma para 1000 observaciones del estimador tau_11, con tamaño de muestra n=10', col='blue')
#notamos que se concentra bastante el histograma en una vecindad de 3, que es theta, el parametro que queremos estimar
plot(density(r_12), lty=1)
plot(density(r_12), lty=4, col='green', main='Densidad de la variable aleatoria tau_11 con tamano de muestra n=10')
#media
mean(r_12)
#
#ahora two figures in one plot
plot(density(r_11), col='blue', main="Densidad de tau_11 para distintos tamaños de muestra", xlim=c(1,7),ylim=c(0,1.5), xlab='valores que toma tau_11')
lines(density(r_12), col='red')
legend(5,0.8, fill=c('blue','red') , c('n=10', "n=100"), cex=0.6 ) #las primeras entradas son las coordenadas donde queremos colocar la leyenda , tambien se puede poner tipo 'topright'
#observacion: estas densidades se constuyeron a partir de mil observaciones de cada estimador, notamos que no son tan suaves y son un poco deformes justo porque 
#mil observaciones no son tantas. Si hicieramos unas 10mil realizaciones de cada estimador para constuir su distribucion de muestreo
#entonces quiza tendriamos lineas mas suaves
#ahora vamos con nuestro segundo estimador de theta, tau12
#n=10
n=10
r_21=1:10000 #generamos vector donde guardaremos las realizaciones de tau12. #aqui si hagamos 10mil realizaciones
for(j in 1:10000){
  simulado=rexp(n, rate=3)
  cuadra=simulado^2 #es un vector en el que cada realizacion se eleva al cuadrado
  tau12=sqrt(2)/sqrt(sum(cuadra)/n)
  r_21[j]=tau12
}
hist(r_21, main='Histograma de 10 mil realizaciones de tau_12 con tamaño de muestra n=10', xlab='observaciones de tau_12', ylab='Frecuencia', ylim=c(0,4000))
plot(density(r_21), main='Densidades de tau_12 construidas a partir de 10 mil realizaciones de tau_12 con distintos tamaños de muestra(n)', col='orange', ylim=c(0,1.5), xlab='valores que toma tau_12')
#n=100
n=100
r_22=1:10000 #generamos vector donde guardaremos las realizaciones de tau12. #aqui si hagamos 10mil realizaciones
for(j in 1:10000){
  simulado=rexp(n, rate=3)
  cuadra=simulado^2 #es un vector en el que cada realizacion se eleva al cuadrado
  tau12=sqrt(2)/sqrt(sum(cuadra)/n)
  r_22[j]=tau12
}
lines(density(r_22), col='green3')
legend(6,0.5, fill=c('orange','green3'), c('n=10', "n=100"))
#
#plot comparativo
#
#tau11
n=10
r_11=1:10000
for(i in 1:10000){
  sim=rexp(n,rate=3) #generamos una observacion de nuestra muestra aleatoria de tamano n
  obs=1/mean(sim) #obtenemos una observacion de nuestra estadistica (estimador de theta) tau_11
  r_11[i]<-obs
}
plot(density(r_11), main='Densidades de distintos estimadores de theta con \n tamaño de muestra n=10, construidas a partir de 10 mil observaciones de cada estimador respectivamente', col='blue', xlab='valores observados', ylab='valor de la función de densidad')
r_21=1:10000 #generamos vector donde guardaremos las realizaciones de tau12. #aqui si hagamos 10mil realizaciones
for(j in 1:10000){
  simulado=rexp(n, rate=3)
  cuadra=simulado^2 #es un vector en el que cada realizacion se eleva al cuadrado
  tau12=sqrt(2)/sqrt(sum(cuadra)/n)
  r_21[j]=tau12
}
lines(density(r_21), col='black')
legend(5,0.3, fill=c('blue', 'black'), c('tau_11', 'tau_12'), cex=0.7, title='Estimadores de theta') #cex sirve para ajustar el tamaño de la leyenda ES MUY UTIL
#title es para ponerle titulo a la leyenda
#observacion: tau_11 acumula mas probabilidad en una vecindad de 3 por lo que es mejor estimador, en ese sentido
#comparemos el sesgo
mean(r_11)
mean(r_21)
#comparemos la varianza
var(r_11)
var(r_21)
?var
#concluimos que es mejor estimador tau_11 que tau_12 pues su sesgo es menor(se acerca mas al parametro que queremos estimar)
#y tiene menor varianza
#Nota: estas conclusiones pueden variar dependiendo de las observaciones obtenidas cada vez que corramos el codigo, pero en general asi son
#para tener todo el analisis en un mismo canal puede servir la fucion set.seed de R
#https://r-coder.com/fijar-semilla-r/ explica mas issues sobre el tema
#c aux
c=c(1,2,3)
c^2 #escupe un vector con cada entrada elevada al cuadrado. Esto es una chulada de R. Ya no es necesario construir un vector asi con un for
#
#
#ahora vamos con tau2(theta)=F(x|theta)
#comenzamos con tau_21
n=10 #tamano de muestras 
x=seq(0,2,1/100) #formamos particion del intervalo [0,2], con subintervalos de longitud 0.01
k=length(x)
k
simula=1:10 #aqui guardamos realizacion de nuestra estadistica en la cual se basa el estimador de tau2
for(j in 1:10){
  simula[j]=mean(rexp(n, rate=3))
}
f<-matrix(nrow=201,ncol=10) #aqui guardaremos los vectores que contienen los puntos a plotear, a partir de las 10 realizaciones de la estadistica
print(f) 
for(j in 1:10){
  for(i in 1:201){
    f[i,j]=1-exp(-x[i]/simula[j])
  }
}

plot(x,1-exp(-3*x), type='l',lty=1,lwd=5, col='red', main='Aproximaciones a la función de distribución exponencial\n usando el estimador tau_21 \n en el intervalo [0,2], n=10 tamaño de muestra', xlab='valores de x', ylab='y') #es la funcion de distrib excata que queremos aproximar
for (j in 1:10){
  lines(x, f[,j], type="l", col='gray')
}
legend(1,0.4, fill=c('red','gray'), c('distribución real', 'aproximaciones'))
help(plot)
#
n=100
simula=1:10 #aqui guardamos realizacion de nuestra estadistica en la cual se basa el estimador de tau2
for(j in 1:10){
  simula[j]=mean(rexp(n, rate=3))
}
f<-matrix(nrow=201,ncol=10) #aqui guardaremos los vectores que contienen los puntos a plotear, a partir de las 10 realizaciones de la estadistica
print(f) 
for(j in 1:10){
  for(i in 1:201){
    f[i,j]=1-exp(-x[i]/simula[j])
  }
}

plot(x,1-exp(-3*x), type='l',lty=1,lwd=5, col='green4', main='Aproximaciones a la función de distribución exponencial\n usando el estimador tau_21 \n en el intervalo [0,2], n=100 tamaño de muestra', xlab='valores de x', ylab='y') #es la funcion de distrib excata que queremos aproximar
for (j in 1:10){
  lines(x, f[,j], type="l", col='gray')
}
legend(1,0.4, fill=c('green4','gray'), c('distribución exacta', 'aproximaciones'))
#ahora vamos con la func de distribucion empirica
x=seq(0,2,1/100) #formamos particion del intervalo [0,2], con subintervalos de longitud 0.01
k=length(x)
k
x
aproximaciones=matrix(nrow=k,ncol=10) #aqui  guardaremos 10 simulaciones de la func de distribucion empirica en el intervalo [0,2]
aproximaciones
#si quisieramos llenarla de ceros inicialmente:
for(i in 1:k){
  for(j in 1:10){
    aproximaciones[i,j]<-0
  }
}
aproximaciones
n=10
for(q in 1:10){ 
  simulacion=rexp(n,rate=3)
  for(w in 2:k){ #podemos inicializar en 2, pues sabemos que ninguna Xi en la m.a. sera menor a cero
    empiric<-0
    for(z in 1:n){
      if(simulacion[z]<=x[w]){
        empiric<-empiric+1
      }
    }
    empiric=empiric/n
    aproximaciones[w,q]<-empiric
  }
}
plot(x,1-exp(-3*x), col='red',type='l', lwd=5, main='Simulaciones de función de distribucion empírica \n con muestras aleatorias de tamaño n=10', xlab='x', ylab='F(x)')
for(j in 1:10){
  lines(x, aproximaciones[,j], col='black', lwd=1, type='l')
}
legend(1.1,0.4, fill=c('black',"red"), c('Simulaciones de función \n de distribución empirica','Función de distribucion de v.a. Exp(3)'), cex=0.9)
aproximaciones[,2]
simulacion

n=100
for(q in 1:10){ 
  simulacion=rexp(n,rate=3)
  for(w in 2:k){ #podemos inicializar en 2, pues sabemos que ninguna Xi en la m.a. sera menor a cero
    empiric<-0
    for(z in 1:n){
      if(simulacion[z]<=x[w]){
        empiric<-empiric+1
      }
    }
    empiric=empiric/n
    aproximaciones[w,q]<-empiric
  }
}
plot(x,1-exp(-3*x), col='red',type='l', lwd=5, main='Simulaciones de función de distribucion empírica \n con muestras aleatorias de tamaño n=100', xlab='x', ylab='F(x)')
for(j in 1:10){
  lines(x, aproximaciones[,j], col='black', lwd=1)
}
legend(1.3,0.4, fill=c('black',"red"), c('Simulaciones de función \n de distribución empirica','Función de distribucion de v.a. Exp(3)'), cex=0.7)

