########
#########
#Ejercicios Tema 6
#Cargar BSDA
#--------------------------------
#Ejercicio 1
#/////////////////////////////////////

#Datos
# 5 años atras, me_0 = 7.5 (h/día)
# n = 8
#H0 -> Q_0.5 > 7.5
#H1 -> Q_0.5 < 7.5

#////////////////////////////////////
#Test de los signos
datos <- c(7.2, 8.3, 5.6, 7.4, 7.8, 5.2, 9.1, 5.8)
SIGN.test(datos, md = 7.5, alternative = "less",conf.level = 0.1)

pbinom(q = 3,size = 8,prob = 0.5,lower.tail = TRUE)

#No tenemos evidencias para rechazar H0

#Wilcoxon
Rangos <- abs(datos-7.5)
wilcox.test(x = datos,mu = 7.5,alternative = "less")

#---------------------------------------------------------------------
#Ejercicio 2

#///////////////////////////////////////////

#me_0 = 0
# n = 10
#H0 -> Q_0.5 = 0
#H1 -> Q_0.5 =! 0

#///////////////////////////////////////////
datos <- c(-0.8, -0.7, -0.4, 1.1, 1.2, 1.5, 1.7,1.8, 1.9, 2.1)
SIGN.test(x = datos, md = 0, alternative = "two.sided")

#No tenemos evidencia para rechazar H0

#wilcoxon
wilcox.test(x = datos, mu = 0, alternative = "two.sided")

boxplot(datos)
#Vemos que no es simétrica la distribución por tanto nos quedamos con la anterior conclusión

#------------------------------------------------------------------
#Ejercicio 4

#/////////////////////////////////////////////////

# p_0 = 0.35   n = 225 (65 leen por la ciudad)
# H0 = p >= p_0
# Y ~ "lee el periódico en la ciudad" Como variable binaria si se cumple o no la condición
#/////////////////////////////////////////////////

binom.test(x = 65, n = 225,p = 0.35,alternative = "less")

#ejercicio 6
binom.test(x = 12, n = 60,p = 0.15,alternative = "greater")

#Ejercicio 7
x <- c(41 ,42 ,48 ,38 ,38 ,45 ,21 ,28 ,29 ,14)
y <- c(37 ,39 ,31 ,39 ,34 ,47 ,19 ,30 ,25 ,8)
dif <- x - y
SIGN.test(x = dif, md = 0,alternative = "less")
SIGN.test()


#tema 7
oij = c