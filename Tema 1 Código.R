########################
#
#    Tema 1
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

#Ejemplo 1.6.
#----------------------------

# * estimador = 33.3
# * parametro = 30
# * desv_tip = 5
# * n = 10

discrepancia(33.3,30,5,10)


#Ejemplo 1.7
#----------------------------

#Forma 1:
# Dado que pnorm(x) = P(X <= x), 1 - pnorm(x) = P(X > x)

(pvalor = 1 - pnorm(2.807))

#Forma 2:
# Indicar en pnorm(x) el parametro "lower.tail = F", para que 
# de P(X > x) en lugar de P(X <= x)

(pvalor_v2 = pnorm(2.807,lower.tail = F))


# Probabilidad H0
#===============================

PrH0Datos <- function(pvalor){
  return(-exp(1)*pvalor*log(pvalor)/(1-exp(1)*pvalor*log(pvalor)))
}

#Ejemplos tabla diapositiva 31:

PrH0Datos(0.1)
PrH0Datos(0.05)
PrH0Datos(0.01)