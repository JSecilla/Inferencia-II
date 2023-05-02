############

########

#Ejercicios Tema 5
##### 
#Problema 1
#Se lanzan 5 monedas 320 veces, con los siguientes resultados Maquina de Galton
 
# Y ~ Be (0.5) cara o cruz
# X ~Bi(5, 0.5) es la distribución del estudio

frecuencias <- c(30,60,120,80,20,10)
Ncara <- 0:5
probs <- dbinom(x= Ncara, size = 5,prob = 0.5)
esperado <- 320*probs

estadistico <- sum((frecuencias - esperado)^2/esperado)

pchisq(estadistico,df = 5,lower.tail = F)
## Esto seria si lo hacemos a mano

#Más rapido
chisq.test(x = frecuencias, p = probs, rescale.p = F)

#como p-valor < alfa rechazamos H0, por tanto aceptamos H1 y diremos que las monedas están trucadas

########
########
#Problema 2
frecuencias <- c(5,35,8,152)
probs <- c(0.04,0.23,0.10,0.63)
esperado <- 200*probs
chisq.test(x=frecuencias, p = probs, rescale.p = F)
#como p-valor < alfa rechazamos H0, por tanto aceptamos H1 y diremos que no se distribuyen igual

########
########
#Problema 3 
frecuencias <- c(75,125,70,80,135,115)
probs <- c(rep(1/6,6))
esperados <- 600*probs
chisq.test(x=frecuencias, p = probs, rescale.p = F)
#como p-valor < alfa rechazamos H0, por tanto aceptamos H1 y diremos que si influye el color del coche a la hora del númro de accidentes


#########
#########
#Problema 4
gasto <- c(10, 14, 15, 20, 30, 45, 45, 60, 85, 120, 130, 200, 200, 270, 270)
lillie.test(gasto)
#Como p-valor > alfa, no rechazamos H0
histogram(gasto)


#########
#########
#Ejercicio voluntario
datos <- c(-16,7,12,-1.6,-11,3.2,12,3.8,-4.5,-9.1,7.2,15.7,-3.3,-16.6,5.8,-15.4,16.6,-7.6)
lillie.test(datos)
mean(datos)
sd(datos)


#########
#########
#Problema 5
#Queremos ver si los datos siguen una exponencial de media 2, es decir exp(1/2)

paux <- pexp(0:5,1/2)
pinter <- diff(paux)

pinter <- c(pinter,pexp(5,1/2,lower.tail = FALSE))
esperado <- pinter * 70

datos <- c(30,23,6,5,6,0)
chisq.test(x = datos, p = pinter,rescale.p = FALSE)

sum((datos-esperado)^2/esperado)

#En general solo hay que hacer las probabilidades teoricas, con el vector de datos

#########
#------------
#Ejercicio 6

datos <- c(0.01, 0.10, 0.19, 0.26, 0.28, 0.32, 0.36, 0.39, 0.42, 0.48,
           0.51, 0.58, 0.62, 0.65, 0.68, 0.76, 0.83, 0.88, 0.92, 0.96)
ks.test(datos,"punif",0,1)

raiz_cuadr_datos <- sqrt(datos)
ks.test(raiz_cuadr_datos,"punif",0,1)

##
###########
#Ejercicio 7

datos <- c(10.39, 10.66, 10.12, 10.32, 10.25, 10.91, 10.52, 10.83,
           10.72, 10.28, 10.35, 10.46, 10.54, 10.72, 10.23, 10.18,
           10.62, 10.49, 10.32, 10.61, 10.64, 10.23, 10.29, 10.78,
           10.81, 10.39, 10.34, 10.62, 10.75, 10.34, 10.41, 10.8,
           10.64, 10.53, 10.31, 10.46, 10.47, 10.43, 10.57, 10.74)

ks.test(datos,"pnorm",10.5,0.1)
#vemos que si ha habido desajuste y no sale esa normal
lillie.test(datos)
cat("Sigue una normal de media",mean(datos), "y desviación típica",sd(datos))


############
############
#Ejercicio 8
frec_obs <- c(8,7,3,5,9,11,6,4,7)
prob <- rep(1/9,9)
chisq.test(x = frec_obs,p = prob, rescale.p = F)
#No podemos rechazar que siga una distribucion uniforme (1/9)

############
############
frec_obs <- c(4,6,7,5,5,8)
p <- rep(1/6,6)
chisq.test(x = frec_obs,p = p,rescale.p = F)


