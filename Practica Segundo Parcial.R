#Repaso segundo Parcial
#Jesús Secilla Martínez

#Primero cargamos los datos y creamos variables con las distintas clases
#Ya que nos será de mucha ayuda de cara a resolver los ejercicios
clase1 <- `datos_05042022.(1)`[`datos_05042022.(1)`$class==1,]
clase2 <- `datos_05042022.(1)`[`datos_05042022.(1)`$class==2,]
datos <- `datos_05042022.(1)`
# 1a-------------------------------------------------------------
#Estudiar si la V1 para los datos de la clase 1 sigue:
# CONTRASTE A REALIZAR
# H0 ~ X = Y                Dónde X es la distribución teórica de V1 en la clase 1
# H1 ~ X =! Y                     Y es la distribución que contrastamos en cada apartado

#Normal media 6 y Dv 0.3
ks.test(clase1$V1,"pnorm",6,0.3)
#Como p-valor ~ 0 rechazamos H0

ks.test(clase1$V1,"pnorm",6.3,0.4)

ks.test(clase1$V1,"pchisq",3)

#una poisson lambda = 3

hist(clase1$V1)
#Ahora intentamos separarlos en intervalos equiprobables
Int1 <- sum(clase1$V1 <=2)
Int2 <- sum(clase1$V1 > 2 & clase1$V1 <=6)
Int3 <- sum(clase1$V1 > 6)
length(clase1$V1)

p0a2 <- ppois(q = 2, lambda = 3)
p2a6 <- ppois(q = 6,lambda = 3) - p0a2
p6inf <- 1 - (p0a2 + p2a6)

chisq.test(x = c(Int1,Int2,Int3), p = c(p0a2,p2a6,p6inf), rescale.p = F)
#Como p-valor es menor que los alfa usuales, rechazamos H0, por lo que tampoco sigue una poiss

ks.test(clase1$V1,"pexp",1/2)
#Vemos que no podemos rechazar que siga una exponencial de media 2

lillie.test(x = clase1$V1)

# 1b-----------------------------------------------------------------------
#Nos piden contrastar lo mismo pero para los datos de la clase 2
#CONTRASTE A REALIZAR
# H0 ~ X = Y                Dónde X es la distribución teórica de V1 en la clase 2
# H1 ~ X =! Y                     Y es la distribución que contrastamos en cada apartado

ks.test(clase2$V1,"pnorm",6,0.3)
#Como p-valor ~ 0 rechazamos H0

ks.test(clase2$V1,"pnorm",6.3,0.4)

ks.test(clase2$V1,"pchisq",3)

#una poisson lambda = 3

hist(clase2$V1)
#Ahora intentamos separarlos en intervalos equiprobables
Int1 <- sum(clase2$V1 <=1)
Int2 <- sum(clase2$V1 > 1 & clase2$V1 <=2)
Int3 <- sum(clase2$V1 > 2 & clase2$V1 <=3)
Int4 <- sum(clase2$V1 > 3)
length(clase2$V1)

p0a1 <- ppois(q = 1, lambda = 3)
p1a2 <- ppois(q = 2,lambda = 3) - p0a1
p2a3 <- ppois(q = 3,lambda = 3) - p0a1 - p1a2
p3inf <- 1 - (p0a1 + p1a2 + p2a3)

chisq.test(x = c(Int1,Int2,Int3,Int4), p = c(p0a1,p1a2,p2a3,p3inf), rescale.p = F)
#Como p-valor es menor que los alfa usuales, rechazamos H0, por lo que tampoco sigue una poiss

ks.test(clase2$V1,"pexp",1/2)
#Vemos que no podemos rechazar que siga una exponencial de media 2

lillie.test(x = clase2$V1)

# 2a--------------------------------------------
#CONTRASTE A REALIZAR
# H0 ~ ME_v1 = 2               Dónde ME_V1 es mediana poblacional de la variable V1
# H1 ~ ME_V1 =! 2                    
V1 <- datos$V1
SIGN.test(x = datos$V1, md = 2, alternative = "two.sided")
#No podemos rechazar que sea distinta de 2
boxplot(V1) #Vemos que no es simétrica
# 2b--------------------------------------------
#CONTRASTE A REALIZAR
# H0 ~ ME_v2 <= 4               Dónde ME_V2 es mediana poblacional de la variable V2
# H1 ~ ME_V2 > 4  
SIGN.test(x = datos$V2, md = 4, alternative = "greater")
boxplot(datos$V2) #Vemos que no es simétrica

#como pVALOR  igual 1, no podemos rechazar H0

# 2c-------------------------------------------------------
#CONTRASTE A REALIZAR
# H0 ~ ME_v3 >= 6               Dónde ME_V3 es mediana poblacional de la variable V3
# H1 ~ ME_V3 < 6 
boxplot(datos$V3) #Vemos que parece simétrica, por lo que podemos aplicar Wilcoxon
wilcox.test(datos$V3,mu = 6, alternative = "less")
SIGN.test(datos$V3,md = 6, alternative = "less")
#Vemos que pvalor = 1 en ambos, por lo que no podemos rechazar H0

# 2d----------------------------------------------------------
#CONTRASTE A REALIZAR
# H0 ~ ME_v2.2 = 5               Dónde ME_V2.2 es mediana poblacional de la variable V2 en la clase 2
# H1 ~ ME_V2.2 =! 5  
 boxplot(clase2$V2) #vemos que no parece simétrica
 SIGN.test(clase2$V2,md = 5, alternative = "two.sided" )
 # Podemos afirmar que la mediana de esta variable no es igual a 5
# 2e-------------------------------------------------------
 #CONTRASTE A REALIZAR
 # H0 ~ p^ >= 0.8         Dónde p^ es la prob teórica de que los datos de V1 sean >= que 2
 # H1 ~ p^ <  0.8  
mayoresa2 <- sum(datos$V1 >= 2)
binom.test(x = mayoresa2, n = length(datos$V1), p = 0.8, alternative = "less")
#como p-valor es casi 0, rechazamos H0......


# 2f---------------------------------------------------------------
#CONTRASTE A REALIZAR
# H0 ~ p^ <= 0.3         Dónde p^ es la prob teórica de que los datos de V1 sean < que 4.7
# H1 ~ p^ >  0.3 
menoresq <- sum(datos$V1 < 4.7) 
binom.test(x = menoresq,n = length(datos$V1),p = 0.3,alternative = "greater")
#Rechazar que como mucho un 30% de los datos de V1 sean menores que 4.7

# 3a---------------------------------------------
# CONTRASTE A REALIZAR
# H0 ~ X = Y                Dónde X es la distribución teórica de V1 en la clase 1
# H1 ~ X =! Y                     Y es la distribución teórica de V1 en la clase 2

ks.test(clase1$V1,clase2$V1)
plot(ecdf(clase1$V1), xlim = c(min(clase1$V1,clase2$V1),max(clase1$V1,clase2$V1)))
lines(ecdf(clase2$V1), col = "green")
wilcox.test(clase1$V1,clase2$V1)
wilcox_test(c(clase1$V1,clase2$V1)~as.factor(c(rep(1,length(clase1$V1)),rep(2,length(clase2$V1)))))

#Para todos los niveles de significación normales rechazamos que se distribuya igual
# 3b----------------------------------------------
# CONTRASTE A REALIZAR
# H0 ~ X = Y                Dónde X es la distribución teórica de V2 en la clase 1
# H1 ~ X =! Y                     Y es la distribución teórica de V2 en la clase 2
ks.test(clase1$V2,clase2$V2)
plot(ecdf(clase1$V2), xlim = c(min(clase1$V2,clase2$V2),max(clase1$V2,clase2$V2)))
lines(ecdf(clase2$V2), col = "green")
wilcox.test(clase1$V2,clase2$V2)
wilcox_test(c(clase1$V2,clase2$V2)~as.factor(c(rep(1,length(clase1$V2)),rep(2,length(clase2$V2)))))
#No podemos rechazar que V2 se distribuye igual en ambas clases 
# 3c----------------------------------------------
# CONTRASTE A REALIZAR
# H0 ~ X = Y                Dónde X es la distribución teórica de V3 en la clase 1
# H1 ~ X =! Y                     Y es la distribución teórica de V3 en la clase 2
ks.test(clase1$V3,clase2$V3)
plot(ecdf(clase1$V3), xlim = c(min(clase1$V3,clase2$V3),max(clase1$V3,clase2$V3)))
lines(ecdf(clase2$V3), col = "green")
wilcox.test(clase1$V3,clase2$V3)
wilcox_test(c(clase1$V3,clase2$V3)~as.factor(c(rep(1,length(clase1$V3)),rep(2,length(clase2$V3)))))
#Rechazamos que V3 se distribuya igual en ambas clases