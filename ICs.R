# construcción de intervalos de confianza
getwd()
#paquetes que vamos a usar
#para simbolos fancy 
install.packages("latex2exp")
library(latex2exp)
install.packages('ggplot2') #instalamos ggplot2
library('ggplot2') #cargamos ggplot2
install.packages('plotrix')
library("plotrix")
inter=seq(-1,5,1/100) #intervalo donde vamos a explorar
print(inter)
l=length(inter)
n=10
q1=qchisq(p=0.05/2, df=2*n) #q1 es t.q. se acumula .025 de prob a la izquierda y 0.975
#a la derecha
q1
q2=qchisq(p=0.975, df=2*n)
q2
#vamos a simular 100 realizaciones de  intervalos aleatorios para cada tamaño de muestra
#y para cada cantidad pivotal
cotaInf1=1:100 #aqui guardamos las cotas inferiores de cada intervalo aleatorio
cotaSup1=1:100 #aqui guardamos las cotas superiores de cada intervalo aleatorio
for(k in 1:100){ 
  simulado=rnorm(n,mean=2,sd=2) #generamos muestra de tamano n para constuir el IC
  indica=1:l #vector auxiliar para construir el IC. Nos va a decir que valores de 
  #mu tomar para construir el IC
  #quiero que quede lleno de ceros
  for (j in 1:l){
    indica[j]=0
  }
  for(m in 1:l){#agarramos una mu y nos preguntamos si vive o no en el 
    #intervalo de confianza
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
  u=which(indica==1) #nos ayuda a identificar que valores de m viven en el
  #intervalo aleatorio
  cotaInf1[k]=inter[min(u)]
  cotaSup1[k]=inter[max(u)]
}
cotaInf1
cotaSup1
print(cotaInf1[2])
print(cotaSup1[2])
#esto nos ayuda a leer la realizacion del segundo intervalo aleatorio simulado.
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
#ahora vamos con muestras de tama;o n=100 
n=100
q1=qchisq(p=0.05/2, df=2*n) #q1 es t.q. se acumula .025 de prob a la izquierda y
#0.975 a la derecha
q1
q2=qchisq(p=0.975, df=2*n)
q2
#vamos a simular 100 realizaciones de  intervalos aleatorios para cada tamaño de
#muestra y para cada cantidad pivotal
cotaInf=1:100 #aqui guardamos las cotas inferiores de cada intervalo aleatorio
cotaSup=1:100 #aqui guardamos las cotas superiores de cada intervalo aleatorio
for(k in 1:100){ 
  simulado=rnorm(n,mean=2,sd=2) #generamos muestra de tamano n para constuir el IC
  indica=1:l #vector auxiliar para construir el IC. Nos va a decir que valores de
  #mu tomar para construir el IC
  #quiero que quede lleno de ceros
  for (j in 1:l){
    indica[j]=0
  }
  for(m in 1:l){#agarramos una mu y nos preguntamos si vive o no en el intervalo
    #de confianza
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
  u=which(indica==1) #nos ayuda a identificar que valores de m viven en el
  #intervalo aleatorio
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
#siempre tienen la misma longitud estos intervalos pues su longitud no depende de
#la realizacion de la muestra (observada).
#primero comparemos las longitudes de los intervalos
mean(cotaSup1-cotaInf1)
cotSup1[1]-cotIn1[1]
cotSup1[2]-cotIn1[2]
mean(cotaSup-cotaInf)
mean(cotSup-cotIn)
cotSup[1]-cotIn[1]
#por lo tanto, los intervalos de confianza construidos con la cantidad pivotal
#tienen longitud menor,
#y de acuerdo con ese criterio, son mejores.
#otro criterio es ver y comparar cuantos intervalos contienen al valor real que
#queremos estimar, osea mu=2
#si realizamos algunas simulaciones y corremos variaza veces el codigo, notamos que
#en general suele ser mejor cantidad pivotal
#Q2 que Q1, pues en general los intervalos de confianza constuidos con Q2 contienen
#al valor mu=2 en mas ocasiones
#
#ahora vamos a hacer plots de los intervalos de confianza
y=1:100
for(i in 1:100){
  y[i]=2 #es un vector lleno de dos para que ponga un puntito en el numero que queremos
  #estimar en el IC
}
#Q1, n=10
data1=data.frame(x=1:100,y=y, lower=cotaInf1, upper=cotaSup1)
data1
#existen dos modos de hacer el plot que queremos usando distintos paquetes de R
#haremos los dos
ggplot(data1, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))
#Q2, n=10
data2=data.frame(x=1:100,y=y, lower=cotIn1, upper=cotSup1)
#usando el segundo metodo
plotCI(x=data2$x,y=data2$y,li=data2$lower,ui=data2$upper,) #usando plotrix
#Nice
#Q1, n=10
data1=data.frame(x=1:100,y=y, lower=cotaInf1, upper=cotaSup1)
data1
plotCI(x=data1$x,y=data1$y,li=data1$lower,ui=data1$upper,
       main=TeX('Intervalos de confianza obtenidos con $Q_{1}$ como cantidad pivotal,
                n=10 como tamaño de muestra para cada intervalo'), xlab='número de
                intervalo aleatorio simulado', ylab='intervalo')
#la desventaja de plotrix es que no es muy preciso al mostrar los intervalos 
#de confianza que no contienen a mu=2
#por lo tanto conviene ggplot2, pero manejarlo con cuidado
#va con ggplot2
#Q1, n=10
data1=data.frame(x=1:100,y=y, lower=cotaInf1, upper=cotaSup1) #formamos un data frame 
#con las cotas inferiores y superiores y queremos idemtificar mu=2
data1
ggplot(data1, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title=TeX('Plot de intervalos de de confianza simulados a partir de $Q_1$ \n
                 con n=10 tamaño de muestra'), x='número de intervalo aleatorio
       observado', y='Intervalo aleatoio observado' )
#Q1, n=100
data2=data.frame(x=1:100,y=y, lower=cotaInf, upper=cotaSup)
data2
ggplot(data2, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title=TeX('Plot de intervalos de de confianza simulados a partir de $Q_{1}$
                 \n con n=100 tamaño de muestra'), x='número de intervalo aleatorio
       observado', y='Intervalo aleatoio observado' )
#Q2, n=10
data3=data.frame(x=1:100,y=y, lower=cotIn1, upper=cotSup1)
data3
ggplot(data3, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title=TeX('Plot de intervalos de de confianza simulados a partir de $Q_2$ \n con
                 n=10 tamaño de muestra'), x='número de intervalo aleatorio observado',
       y='Intervalo aleatoio observado' )
#Q2, n=100
data4=data.frame(x=1:100,y=y, lower=cotIn, upper=cotSup)
data4
ggplot(data4, aes(x,y))+
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(title=TeX('Plot de intervalos de de confianza simulados a partir de $Q_2$ \n con 
                 n=100 tamaño de muestra'), x='número de intervalo aleatorio observado',
       y='Intervalo aleatoio observado' )