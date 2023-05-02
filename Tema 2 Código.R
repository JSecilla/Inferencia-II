########################
#
#    Tema 2
#
###########################################

# Funcion de Discrepancia
#===============================

#Funcion tal y como se define en la diapositiva 20 del tema 1, donde:
# * estimador = valor estimado en la muestra
# * parametro = valor del parametro que se da como cierto segun H0
# * desv_tip = desviacion tipica de los datos
# * n = tamano muestral

discrepancia <- function(estimador,parametro,desv_tip,n){
  return((estimador-parametro)/(desv_tip/sqrt(n)))
}



#Ejemplo 2.1
#------------------------

### TEST DE HIPOTESIS:

X = c(208, 206, 210, 199, 202, 196, 198, 209, 211, 204, 206, 197,196, 203, 207)
(estimador = mean(X))

(est = discrepancia(estimador,200,4.5,15))

#Forma 1:
# Dado que pnorm(x) = P(X <= x), 1 - pnorm(x) = P(X > x)

(pvalor = 2*(1 - pnorm(est)))

#Forma 2:
# Indicar en pnorm(x) el parametro "lower.tail = F", para que 
# de P(X > x) en lugar de P(X <= x)

(pvalor_v2 = 2*(pnorm(est,lower.tail = F)))


### INTERVALOS DE CONFIANZA:

ic <- function(estimador,alpha,desv_est,n){
  limiteInferior = estimador-qnorm(alpha/2,lower.tail = F)*desv_est/sqrt(n)
  limiteSuperior = estimador+qnorm(alpha/2,lower.tail = F)*desv_est/sqrt(n)
  return(paste0("El intervalo de confianza para la media es: [",round(limiteInferior,4)," , ",round(limiteSuperior,4),"]"))
}

ic(203.4667,0.05,4.5,15)
ic(203.4667,0.01,4.5,15)




#Ejemplo 2.2
#-------------------------

X = c(208, 206, 210, 199, 202, 196, 198, 209, 211, 204, 206, 197,196, 203, 207)
(estimador = mean(X))

(est = discrepancia(estimador,200,4.5,15))

(pvalor = pnorm(est,lower.tail = F))


#Ejemplo 2.3
#--------------------------

X = c(208, 206, 210, 199, 202, 196, 198, 209, 211, 204, 206, 197,196, 203, 207)
(estimador = mean(X))
(cuasi_desv_est = sd(X))

(est = discrepancia(estimador,200,cuasi_desv_est,15))

#Cuidado, ahora la distribucion es una t_14 (no usamos sd sino cuasi_sd)!!!!!
(pvalor = 2*pt(est,lower.tail = F,df=14)) 



##
# Hacer ejercicio voluntario, cambiar z alfa por t (qt) con n - 1,
# y hacer el contratste bilateral para ambos lados a 0.05, (Ejemplo 2.3)
pt()
pchisq(9.659, 9)

pt(2.5712, 14, lower.tail = FALSE)
qt(0.005, 14, lower.tail = FALSE)

x <- c(208,206,210,199,202,196,198,209,211,204,206,197,196,203,207)
t.test(x)
t.test(x, conf.level = 0.99)

###
###
#Ejercicios Tema 2
 #Una organizacion de consumidores cree que el precio medio del billete de avion entre Canarias
 #y la Peninsula es mayor de los 108 euros que afirma la Asociacion de companias Aereas. Se
 #pregunta a 9 viajeros y se obtiene que la media de los precios de sus billetes es de 128 euros con
 #cuasi-desviacion tipica 30. Se supone que el precio del billete sigue una distribucion normal.
 #Contrastar la hipotesis de partida para ?? = 0.1, 0.05 y 0.01 mediante el p-valor

media <- 128
cuasides <- 30
n <- 9
discrepancia(media, 108, cuasides, n) #2
pt(2,8, lower.tail = F) #0.0402
