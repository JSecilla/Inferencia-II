# Scripts de R Usados - Grupo 6

# En este fichero incluimos las funciones utilizadas para generar los estadísticos, p-valores y gráficas del trabajo.

datos = read.csv(file = "Chemical_Composion_of_Ceramic.csv", header = TRUE, dec = ",", sep = ";")
datos = `Chemical_Composion_of_Ceramic.(2)`

datos[2,16]=abs(datos[2,16])                               # Corregimos un valor negativo de SrO que consideramos una errata.


# Paquetes (necesario cargarlos)

if(!require("asbio")){
  install.packages("asbio")
}
if(!require("fBasics")){
  install.packages("fBasics")
}
if(!require("BSDA")){
  install.packages("BSDA")
}
if(!require("EnvStats")){
  install.packages("EnvStats")
}
if(!require("nortest")){
  install.packages("nortest")
}
if(!require("qpcR")){
  install.packages("qpcR")
}
if(!require("coin")){
  install.packages("coin")
}
if(!require("SuppDists")){
  install.packages("SuppDists")
}

library("asbio")      
library("fBasics")
library("BSDA")
library("EnvStats")
library("nortest")
library("qpcR")
library("coin")
library("SuppDists")


# Funciones (necesario cargarlas)

tukey_statistics <- function(x1,x2,x3,x4,anova) {
  N=length(c(x1,x2,x3,x4))
  B=matrix(NA,nrow=4,ncol=4)
  rownames(B)=c("x1","x2","x3","x4")
  colnames(B)=rownames(B)
  A=matrix(list(), nrow=1, ncol=4)
  A[[1,1]]=x1
  A[[1,2]]=x2
  A[[1,3]]=x3
  A[[1,4]]=x4
  for (i in 1:3) {
    for (j in (i+1):4) {
      B[i,j]=abs(mean(A[[i]])-mean(A[[j]]))/sqrt(RSS(anova)/(2*(N-4)))*1/sqrt(1/length(A[[i]])+1/length(A[[j]]))
    }
  }
  return(B)
}

spearman_rho <- function (x,y) {
  statistic=1-6*sum((rank(x)-rank(y))^2)/(length(x)^3-length(x))
  return(statistic)
}

kendall_tau <- function(x,y) {
  z=0
  for (i in 2:length(x)) {
    for (j in 1:(i-1)) {
      z=z+(sign(x[i]-x[j])*sign(y[i]-y[j]))
    }
  }
  statistic=z/choose(length(x),2)
  return(statistic)
}

kendall_pvalue<-function (x,y,n,alternative=c("two.sided","less","greater")) {
  statistic=kendall_tau(x,y)
  res=0
  for (i in 1:n) {
    X=runif(length(x))
    Y=runif(length(x))
    STATISTIC=kendall_tau(X,Y)
    if (alternative=="less") {
      if (STATISTIC<=statistic) {
        res=res+1
      }
    } else {
      if (STATISTIC>=statistic) {
        res=res+1
      }
    }
  }
  if (alternative=="two.sided") {
    pvalue_greater=res/n
    pvalue=2*min(pvalue_greater,1-pvalue_greater)
  } else {
    pvalue=res/n
  }
  return(pvalue)
}


# Ejercicio 1.a

x=datos$Al2O3

z.test(x,mu=20,alternative="two.sided",sigma.x=sd(x))       # Devuelve estadístico y p-valor correctos.

z.test(x,mu=20,alternative="less",sigma.x=sd(x))            # Devuelve estadístico y p-valor correctos.

z.test(x,mu=20,alternative="greater",sigma.x=sd(x))         # Devuelve estadístico y p-valor correctos.


# Ejercicio 1.b

x=datos$ZrO2

z.test(x,mu=150,alternative="two.sided",sigma.x=sd(x))      # Devuelve estadístico y p-valor correctos.



# Ejercicio 2.a

x=datos$Al2O3

varTest(x,alternative="two.sided",sigma.squared=32)         # Devuelve estadístico y p-valor correctos.


# Ejercicio 2.b

x=datos$ZrO2

varTest(x,alternative="two.sided",sigma.squared=4500)       # Devuelve estadístico y p-valor correctos.



# Ejercicio 3.a


x=(datos$CaO)[1:44]
y=(datos$CaO)[45:88]
d=x-y

z.test(d,mu=0,alternative="two.sided",sigma.x=sd(d))        # Devuelve estadístico y p-valor correctos.

z.test(d,mu=0,alternative="less",sigma.x=sd(d))             # Devuelve estadístico y p-valor correctos.

z.test(d,mu=0,alternative="greater",sigma.x=sd(d))          # Devuelve estadístico y p-valor correctos.


# Ejercicio 3.b

x=(datos$ZnO)[1:44]
y=(datos$ZnO)[45:88]
d=x-y

z.test(d,mu=0,alternative="two.sided",sigma.x=sd(d))        # Devuelve estadístico y p-valor correctos.



# Ejercicio 4.a

x=(datos$CuO)
y=(datos$PbO2)
d=x-y

z.test(d,mu=0,alternative="two.sided",sigma.x=sd(d))        # Devuelve estadístico y p-valor correctos.

z.test(d,mu=0,alternative="less",sigma.x=sd(d))             # Devuelve estadístico y p-valor correctos.

z.test(d,mu=0,alternative="greater",sigma.x=sd(d))          # Devuelve estadístico y p-valor correctos.


# Ejercicio 4.b

x=(datos$CuO)[1:44]
y=(datos$PbO2)[1:44]
d=x-y

z.test(d,mu=0,alternative="two.sided",sigma.x=sd(d))        # Devuelve estadístico y p-valor correctos.

z.test(d,mu=0,alternative="less",sigma.x=sd(d))             # Devuelve estadístico y p-valor correctos.

z.test(d,mu=0,alternative="greater",sigma.x=sd(d))          # Devuelve estadístico y p-valor correctos.



# Ejercicio 5.a

x1=(datos$SrO)[14:44]
x2=(datos$SrO)[58:88]
x3=(datos$SrO)[1:13]
x4=(datos$SrO)[45:57]
x=c(x1,x2,x3,x4)

Block <- c(rep(1,length(x1)), rep(2,length(x2)), rep(3,length(x3)), rep(4,length(x4)))
Block <- factor(Block)
anova <- aov(x ~ Block)

summary(anova)                                              # Devuelve estadístico y p-valor correctos.
boxplot(x ~ Block)                                          # Devuelve diagrama de cajas de (x1,x2,x3,x4)


# Ejercicio 5.b

x1=(datos$SrO)[14:44]
x2=(datos$SrO)[58:88]
x3=(datos$SrO)[1:13]
x4=(datos$SrO)[45:57]
x=c(x1,x2,x3,x4)

Block <- c(rep(1,length(x1)), rep(2,length(x2)), rep(3,length(x3)), rep(4,length(x4)))
Block <- factor(Block)
anova <- aov(x ~ Block)

tukey_statistics(x1,x2,x3,x4,anova)                         # Devuelve la matriz de estadísticos de Tukey.
TukeyHSD(anova)                                             # Devuelve p-valores correctos.
plot(TukeyHSD(anova))                                       # Devuelve intervalos de confianza de cada par de medias.


# Ejercicio 5.c

x1=(datos$Y2O3)[14:44]
x2=(datos$Y2O3)[58:88]
x3=(datos$Y2O3)[1:13]
x4=(datos$Y2O3)[45:57]
x=c(x1,x2,x3,x4)

Block <- c(rep(1,length(x1)), rep(2,length(x2)), rep(3,length(x3)), rep(4,length(x4)))
Block <- factor(Block)
anova <- aov(x ~ Block)

summary(anova)                                              # Devuelve estadístico y p-valor correctos.
boxplot(x ~ Block)                                          # Devuelve diagrama de cajas de (x1,x2,x3,x4).


# Ejercicio 5.d

x1=(datos$Y2O3)[14:44]
x2=(datos$Y2O3)[58:88]
x3=(datos$Y2O3)[1:13]
x4=(datos$Y2O3)[45:57]
x=c(x1,x2,x3,x4)

Block <- c(rep(1,length(x1)), rep(2,length(x2)), rep(3,length(x3)), rep(4,length(x4)))
Block <- factor(Block)
anova <- aov(x ~ Block)

tukey_statistics(x1,x2,x3,x4,anova)                         # Devuelve la matriz de estadísticos de Tukey.
TukeyHSD(anova)                                             # Devuelve p-valores correctos.
plot(TukeyHSD(anova))                                       # Devuelve intervalos de confianza de cada par de medias.



# Ejercicio 6.a

x=datos$Fe2O3
ks.test(x,"pnorm",2,sqrt(0.5))                              # Devuelve estadístico y p-valor correctos.

plot(ecdf(x),main="Ajuste de la función de distribución empírica a la distribución de una N (2, 0.5)",xlim=c(0.25,3.75))
z=seq(0,4,0.005)
lines(z,pnorm(z,2,sqrt(0.5)),type="l",col="red")            # Devuelve gráfica del apartado 6.a.


# Ejercicio 6.b

x=datos$Fe2O3
lillie.test(x)                                              # Devuelve estadístico y p-valor correctos.

plot(ecdf(x),main="Ajuste de la función de distribución empírica a la distribución de una N (x, s2)",xlim=c(0.25,3.75))
z=seq(0,4,0.01)
lines(z,pnorm(z,mean(x),sd(x)),type="l",col="blue")         # Devuelve gráfica del apartado 6.b.


# Ejercicio 6.c

x=(datos$MgO)[1:44]
ks.test(x,"pnorm",0.5,sqrt(0.1))                            # Devuelve estadístico y p-valor correctos.

plot(ecdf(x),main="Ajuste de la función de distribución empírica a la distribución de una N (0.5, 0.1)",xlim=c(-0.2,1.2))
z=seq(-0.3,1.3,0.001)
lines(z,pnorm(z,0.5,sqrt(0.1)),type="l",col="red")          # Devuelve gráfica del apartado 6.c.


# Ejercicio 6.d

x=(datos$MgO)[1:44]
lillie.test(x)                                              # Devuelve estadístico y p-valor correctos.

plot(ecdf(x),main="Ajuste de la función de distribución empírica a la distribución de una N (x, s2)",xlim=c(-0.2,1.2))
z=seq(-0.3,1.3,0.001)
lines(z,pnorm(z,mean(x),sd(x)),type="l",col="blue")         # Devuelve gráfica del apartado 6.d.



# Ejercicio 7.a

x=datos$P2O5
boxplot(x)                                                  # Vemos que la distribución no es simétrica.

SIGN.test(x,md=350,alternative="two.sided")                 # Devuelve estadístico y p-valor correctos.


# Ejercicio 7.b

x=(datos$PbO2)[c(14:44,58:88)]

S=SIGN.test(x,md=60,alternative="two.sided")$statistic      
S                                                           # Devuelve el estadístico correcto. 
2*min(pbinom(S,length(x),0.5),1-pbinom(S-1,length(x),0.5))  # Devuelve el p-valor correcto.


S=SIGN.test(x,md=60,alternative="less")  $statistic         
S                                                           # Devuelve el estadístico correcto.
pbinom(S,length(x),0.5)                                     # Devuelve el p-valor correcto.


S=SIGN.test(x,md=60,alternative="greater")  $statistic      
S                                                           # Devuelve el estadístico correcto.
1-pbinom(S-1,length(x),0.5)                                 # Devuelve el p-valor correcto.


# Ejercicio 7.c

y=sum(datos$Rb2O>290)                                       # Devuelve el estadístico correcto.
2*min(binom.test(y,length(datos$Rb2O),p=0.8,alternative="less")$p.value,binom.test(y,length(datos$Rb2O),p=0.8,alternative="greater")$p.value)
                                                            # Devuelve el p-valor correcto (binom.test() con two sided devuelve un resultado inexacto).

# Ejercicio 8.a

x=datos$Na2O
y=datos$MgO

ks.test(x,y)                                                # Devuelve estadístico y p-valor correctos.


W=wilcox.test(x,y)$statistic                                # Devuelve el estadístico correcto.
W                                                           
2*pnorm(abs((W-(length(x)*length(y)/2))/sqrt(length(x)*length(y)*(length(x)+length(y)+1)/12)),lower.tail=F)
                                                            # Devuelve el p-valor correcto.
                                                            

plot(ecdf(x),xlim =c(min(x,y),max(x,y)),main="Funciones de distribución empíricas de Na2O (negro) y MgO (verde)")
lines(ecdf(y),col="green")                                  # Devuelve la gráfica del apartado 8.a.


# Ejercicio 8.b

x=(datos$CuO)[c(1:44,58:88)]
y=(datos$Y2O3)[c(1:44,58:88)]

ks.test(x,y)                                                # Devuelve estadístico y p-valor correctos.


W=wilcox.test(x,y)$statistic                                # Devuelve el estadístico correcto.
W                                                           
2*pnorm(abs((W-(length(x)*length(y)/2))/sqrt(length(x)*length(y)*(length(x)+length(y)+1)/12)),lower.tail=F)
                                                            # Devuelve el p-valor correcto.


plot(ecdf(x),xlim = c(min(x,y),max(x,y)),col="red",main="Funciones de distribución empíricas de CuO (rojo) e Y2O3 (azul)")
lines(ecdf(y),col="blue") 



# Ejercicio 9.a

x=datos$CaO
y=datos$P2O5

spearman_rho(x,y)                                            # Devuelve el estadístico correcto.
2*min(pSpearman(spearman_rho(x,y),length(x)),pSpearman(spearman_rho(x,y),length(x),lower.tail=F))
                                                             # Devuelve el p-valor correcto.

kendall_tau(x,y)                                             # Devuelve el estadístico correcto.
set.seed(2022)
kendall_pvalue(x,y,100,alternative="two.sided")              # Devuelve el p-valor correcto, aproximado por Monte Carlo. En el trabajo se ha usado n=10^6.


plot(x,y,xlab="Porcentaje de peso de CaO",ylab="Partes por millón de P2O5",main="Diagrama de dispersión de CaO y P2O5",pch=19)
                                                            # Devuelve el diagrama de dispersión.

# Ejercicio 9.b

x=datos$TiO2
y=datos$Na2O

spearman_rho(x,y)                                            # Devuelve el estadístico correcto.
2*min(pSpearman(spearman_rho(x,y),length(x)),pSpearman(spearman_rho(x,y),length(x),lower.tail=F))
                                                             # Devuelve el p-valor correcto.

kendall_tau(x,y)                                             # Devuelve el estadístico correcto.
set.seed(2022)
kendall_pvalue(x,y,100,alternative="two.sided")              # Devuelve el p-valor correcto, aproximado por Monte Carlo. En el trabajo se ha usado n=10^6.


plot(x,y,xlab="Porcentaje de peso de TiO2",ylab="Porcentaje de peso de Na2O",main="Diagrama de dispersión de TiO2 y Na2O",pch=19)
                                                             # Devuelve el diagrama de dispersión.


# Ejercicio 10.a

x=datos$Al2O3
y=c()

set.seed(2022)
z=sample(c(1:length(x)),length(x),replace=FALSE)             # Aleatorizamos el orden de la muestra de X.
x=x[z]
for (i in 1:(length(x)/2)) {                                 # Generamos la variable aleatoria Y, agrupando por pares e impares.
  y[i]=((x[2*i-1]-x[2*i])^2)/2
}

z.test(y,mu=32,sigma.x=sd(y),conf.level=0.90,alternative="two.sided")    # Realizamos el contraste mediante TCL.


# Ejercicio 10.b

x=datos$ZrO2
y=c()

set.seed(2022)
z=sample(c(1:length(x)),length(x),replace=FALSE)            # Aleatorizamos el orden de la muestra de X.
x=x[z]
for (i in 1:(length(x)/2)) {                                # Generamos la variable aleatoria Y, agrupando por pares e impares.
  y[i]=((x[2*i-1]-x[2*i])^2)/2
}

z.test(y,mu=4500,sigma.x=sd(y),conf.level=0.90,alternative="two.sided")   # Realizamos el contraste mediante TCL.

# FIN
