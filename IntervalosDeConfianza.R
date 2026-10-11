getwd()
#para simbolos fancy NICEEEEE
install.packages("latex2exp")
library(latex2exp)
#primero vamos a construir intervalos de confianza para mu numericos para n=10, n=100 como tamano de muestra
#de m.a. N(mu,sigmacuadrada), mu=2, sigma^2=4
#usando la cantidad pivotal que nos regala el teorema de cantidades pivotales continuas,
#esta cantidad pivotal no permite un pivoteo analitico en este caso
#tomamos alpha=0.05, i.e. un nivel de confianza de .95=1-alpha
?seq
#por eso necesitamos un apporach numerico
seq(from=0,to=2,by=(2-0)/100) #genera particion del intervalo [0,2] en 100 pedazos, i.e. 100 subintervalos de longiutdes iguales con longitud de 2/100 
length(seq(from=0,to=2,by=(2-0)/100))
#seq es el equivalente a linspace de Matlab
inter=seq(-1,5,1/100)
inter
length(inter)
#es nuestra region a explorar para construir el intervalo de confianza numerico
#cuantiles
#dchisq(x, df, ncp = 0, log = FALSE)
#pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
#qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
#rchisq(n, df, ncp = 0)
n=10
q1=qchisq(p=0.05/2, df=2*n) #q1 es t.q. se acumula .025 de prob a la izquierda y 0.975 a la derecha
q1
q2=qchisq(p=0.975, df=2*n)
q2
#vamos a simular 100 realizaciones de  intervalos aleatorios para cada tamaño de muestra y para cada cantidad pivotal
cotaInf1=1:100 #aqui guardamos las cotas inferiores de cada intervalo aleatorio
cotaSup1=1:100 #aqui guardamos las cotas superiores de cada intervalo aleatorio
for(k in 1:100){ 
  simulado=rnorm(n,mean=2,sd=2) #generamos muestra de tamano n para constuir el IC
  indica=1:601 #vector auxiliar para construir el IC. Nos va a decir que valores de mu tomar para construir el IC
  #quiero que quede lleno de ceros
  for (j in 1:601){
    indica[j]=0
  }
  for(m in 1:601){#agarramos una mu y nos preguntamos si vive o no en el intervalo de confianza
    Q<-0
    for(i in 1:n){
      Q=Q-2*log(pnorm((simulado[i]-inter[m])/2))
    }
    #print(Q)
    if((q1<Q) && (Q<q2)){
      indica[m]=1 #quiere decir que la m sí vive en el intervalo aleatorio
    }
    else{
      indica[m]=0 #entonces la m no esta en el intervalo de confianza
    }
  }
  u=which(indica==1) #nos ayuda a identificar que valores de m viven en el intervalo aleatorio
  cotaInf1[k]=inter[min(u)]
  cotaSup1[k]=inter[max(u)]
}
cotaInf1
cotaSup1
print(cotaInf1[2])
print(cotaSup1[2])
#esto nos ayuda a leer la realizacion del segundo intervalo aleatorio simulado. NICEEE
#
#check de que tiene sentido
for (i in 1:100){
  if(cotaInf1[i]<cotaSup1[i]){
    print(1)
  }
  else{
    print(0)
  }
}
#escupe puros unos :)

#tambien podemos  contar cuantos intervalos de los 100 incluyen al valor de mu=2
cuentas=0
for(j in 1:100){
  if(cotaInf1[j]<2 && 2<cotaSup1[j]){
    cuentas<-cuentas+1
  }
}
print(cuentas)

#
#cosas auxiliares
#informacion sobre vactores 
c=c(1,1,1,2,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,2,2,2,22)
c
max(c)
min(c)
c[c>2]
c[c<2]
c==1
pnorm(0) #pnorm es la famosa phi de una normal, i.e. la funcion de distribucion 
pnorm(1.64)
log(exp(1))
log(0)
log(1)
#log es logaritmo natural i.e con base e, el numero de Euler, e=2.718
exp(1)
c
u=which(c==1) #nos da los indices de las entradas de c que toman el valor igual a 1. Esto es muy util pera
#construir el intervalo de confianza numerico
u<-which(c==1) 
u
min(u)
max(u)
length(c)
c[16]
which(c==2)
for(i in 1:length(u)){
  print(c[u[i]]) #esto nos va a imprimir puros unos
}
#Niceee
#queremos hacer plot del estilo del link de aqui
#https://statisticsglobe.com/draw-plot-with-confidence-intervals-in-r
#https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals
#https://www.youtube.com/watch?v=tU3Wex7Qqk8 ######  <----- Este esta perro
# Este tambien:
#https://www.geeksforgeeks.org/how-to-plot-a-confidence-interval-in-r/
#
#
#
#
#ahora vamos con muestras de tama;o n=100 
n=100
q1=qchisq(p=0.05/2, df=2*n) #q1 es t.q. se acumula .025 de prob a la izquierda y 0.975 a la derecha
q1
q2=qchisq(p=0.975, df=2*n)
q2
#vamos a simular 100 realizaciones de  intervalos aleatorios para cada tamaño de muestra y para cada cantidad pivotal
cotaInf=1:100 #aqui guardamos las cotas inferiores de cada intervalo aleatorio
cotaSup=1:100 #aqui guardamos las cotas superiores de cada intervalo aleatorio
for(k in 1:100){ 
  simulado=rnorm(n,mean=2,sd=2) #generamos muestra de tamano n para constuir el IC
  indica=1:601 #vector auxiliar para construir el IC. Nos va a decir que valores de mu tomar para construir el IC
  #quiero que quede lleno de ceros
  for (j in 1:601){
    indica[j]=0
  }
  for(m in 1:601){#agarramos una mu y nos preguntamos si vive o no en el intervalo de confianza
    Q<-0
    for(i in 1:n){
      Q=Q-2*log(pnorm((simulado[i]-inter[m])/2))
    }
    #print(Q)
    if((q1<Q) && (Q<q2)){
      indica[m]=1 #quiere decir que la m sí vive en el intervalo aleatorio
    }
    else{
      indica[m]=0 #entonces la m no esta en el intervalo de confianza
    }
  }
  u=which(indica==1) #nos ayuda a identificar que valores de m viven en el intervalo aleatorio
  cotaInf[k]=inter[min(u)]
  cotaSup[k]=inter[max(u)]
}
cotaInf
cotaSup
#tambien podemos  contar cuantos intervalos de los 100 incluyen al valor de mu=2
cuentas=0
for(j in 1:100){
  if(cotaInf[j]<2 && 2<cotaSup[j]){
    cuentas<-cuentas+1
  }
}
print(cuentas)
#
#
#
#
#
#vamos a comparar con la otra cantidad pivotal
z=qnorm(0.975)
z
n<-10
cotIn1=1:100
cotSup1=1:100
for(k in 1:100){
  simula=rnorm(n,mean=2,sd=2)
  mediaMuestral=mean(simula)
  cotIn1[k]=mediaMuestral-(2*z)/sqrt(n)
  cotSup1[k]<-mediaMuestral+(2*z)/sqrt(n)
}
cotIn1
cotSup1
cuentas=0
for(i in 1:100){
  if(cotIn1[i]<2 && 2<cotSup1[i]){
    cuentas=cuentas+1
  }
}
print(cuentas)
#Nice
#ahora un tamano de muestra de n=100 para cada intervalo de confianza 
n<-100
cotIn=1:100
cotSup=1:100
for(k in 1:100){
  simula=rnorm(n,mean=2,sd=2)
  mediaMuestral=mean(simula)
  cotIn[k]=mediaMuestral-(2*z)/sqrt(n)
  cotSup[k]<-mediaMuestral+(2*z)/sqrt(n)
}
cotIn
cotSup
cuentas=0
for(i in 1:100){
  if(cotIn[i]<2 && 2<cotSup[i]){
    cuentas=cuentas+1
  }
}
print(cuentas)
#listo. 
#falta hacer analisis visual y analitico
#ya tenemos los intervalos que queremos. Falta interpretarlos y hacer observaciones
cotSup[1]-cotIn[1]
cotSup[2]-cotIn[2]
#siempre tienen la misma longitud estos intervalos pues su longitud no depende de la realizacion de la muestra (observada).
#primero comparemos las longitudes de los intervalos
mean(cotaSup1-cotaInf1)
cotSup1[1]-cotIn1[1]
#por lo tanto, los intervalos de confianza construidos con la cantidad pivotal tienen longitud menor,
#y de acuerdo con ese criterio, son mejores.
#otro criterio es ver y comparar cuantos intervalos contienen al valor real que queremos estimar, osea mu=2
#si realizamos algunas simulaciones y corremos variaza veces el codigo, notamos que en general suele ser mejor cantidad pivotal
#Q2 que Q1, pues en general los intervalos de confianza constuidos con Q2 contienen al valor mu=2 en mas ocasiones
#
#
#ahora vamos a hacer plots de los intervalos de confianza
y=1:100
for(i in 1:100){
  y[i]=2 #es un vector lleno de dos para que ponga un puntito en el numero que queremos estimar en el IC
}
#Q1, n=10
data1=data.frame(x=1:100,y=y, lower=cotaInf1, upper=cotaSup1)
data1
#existen dos modos de hacer el plot que queremos usando distintos paquetes de R
#haremos los dos
#me estoy basando en el video de youtube
install.packages('ggplot2') #instalamos ggplot2
library('ggplot2') #cargamos ggplot2
ggplot(data1, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))

#Q2, n=10
data2=data.frame(x=1:100,y=y, lower=cotIn1, upper=cotSup1)
#usando el segundo metodo
install.packages('plotrix')
library("plotrix")
plotCI(x=data2$x,y=data2$y,li=data2$lower,ui=data2$upper,)

#Nice
#conviene el segundo metodo porque permite modificar mas cosas del plot como poner titulo etc

###
#
##
#va en limpio
##Q1, n=10
data1=data.frame(x=1:100,y=y, lower=cotaInf1, upper=cotaSup1)
data1
plotCI(x=data1$x,y=data1$y,li=data1$lower,ui=data1$upper,main='Intervalos de confianza obtenidos con Q1 como cantidad pivotal, n=10 como tamaño de muestra para cada intervalo', xlab='número de intervalo aleatorio simulado', ylab='intervalo')
#la desventaja de este es que no es muy preciso al mostrar los intervalos de confianza que no contienen a mu=2
#por lo tanto conviene ggplot2, pero manejarlo con cuidado
# y tambien investigando como ajustar y modificar el plot
#cosas utiles de ggplot:
# http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
#
#
#
##
#
#
##
#va con ggplot2, en limpio
#
#Q1, n=10
data1=data.frame(x=1:100,y=y, lower=cotaInf1, upper=cotaSup1)
data1
ggplot(data1, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title='Plot de intervalos de de confianza simulados a partir de Q1 \n con n=10 tamaño de muestra', x='número de intervalo aleatorio observado',y='Intervalo aleatoio observado' )
#Q1, n=100
data2=data.frame(x=1:100,y=y, lower=cotaInf, upper=cotaSup)
data2
ggplot(data2, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title='Plot de intervalos de de confianza simulados a partir de Q1 \n con n=100 tamaño de muestra', x='número de intervalo aleatorio observado',y='Intervalo aleatoio observado' )
#Q2, n=10
data3=data.frame(x=1:100,y=y, lower=cotIn1, upper=cotSup1)
data3
ggplot(data3, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title='Plot de intervalos de de confianza simulados a partir de Q2 \n con n=10 tamaño de muestra', x='número de intervalo aleatorio observado',y='Intervalo aleatoio observado' )

#
#Q2, n=100
data4=data.frame(x=1:100,y=y, lower=cotIn, upper=cotSup)
data4
ggplot(data4, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title='Plot de intervalos de de confianza simulados a partir de Q2 \n con n=100 tamaño de muestra', x='número de intervalo aleatorio observado',y='Intervalo aleatoio observado' )


#para simbolos fancy NICEEEEE
install.packages("latex2exp")
library(latex2exp)
plot(x, y, main = TeX('$\\beta^3, \\beta \\in 1 \\ldots 10$'))

