getwd()
x=seq(0,34,1/10)
x
length(x)
x[171]
y=1:length(x)
for(j in 1:length(y)){
  if(j<=171){
    y[j]<-0
  }
  else{
    y[j]=1
  }
}
y
length(y)
plot(x,y, type='l', col='red', lwd=1, lty=1, xlim=c(0,33))
?plot
#pero el chiste es que se vea como una funcion discontinua en mu=17
plot(x[1:171],y[1:171],type='l', col='gray', lwd=6, lty=1, xlim=c(0,33), ylim=c(-.1,1), main='Convergencia de la potencia de la prueba a la potencia ideal', xlab='valores de mu', ylab='Probabilidad de rechazar H_0')
lines(x[172:341],y[172:341], type='l', col='gray', lwd=6, lty=1, xlim=c(0,33))
#potencia para distintos tamanos de muestra
n=10
potencia1=1:length(x)
for(k in 1:length(x)){
  potencia1[k]=1-pnorm((8.2/sqrt(n)+17-x[k])/(5/sqrt(n)))
}
lines(x,potencia1, col='black', type='l')
n=100
potencia2=1:length(x)
for(k in 1:length(x)){
  potencia2[k]=1-pnorm((8.2/sqrt(n)+17-x[k])/(5/sqrt(n)))
}
lines(x,potencia2, col='blue', type='l')
n=1000
potencia3=1:length(x)
for(k in 1:length(x)){
  potencia3[k]=1-pnorm((8.2/sqrt(n)+17-x[k])/(5/sqrt(n)))
}
lines(x,potencia3, col='green4', type='l')
alfa=1:length(x)
for(i in 1:length(alfa)){
  alfa[i]=0.05
}
lines(x,alfa, col='red', lty=2)
legend(21,0.5, fill=c('gray','black','blue', 'green4', 'red'), c('Potencia ideal', "Potencia de la prueba con n=10", 'Potencia de la prueba con n=100',"Potencia de la prueba con n=1000", TeX('Nivel de significancia $\\alpha =0.05$')), cex=0.8)
# info util para plots

#https://r-coder.com/plot-r/

#para simbolos fancy NICEEEEE
install.packages("latex2exp")
library(latex2exp)
plot(x, y, main = TeX('$\\beta^3, \\beta \\in 1 \\ldots 10$'))
plot(x,y, main=TeX('$\\gamma, \\tau_{11} \\in  \\{1,\\ldots 10\\}$'))
##
