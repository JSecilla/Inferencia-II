#..............
#Ejercicios Tema 3
#
#
######
#Ejercicio 1
#Identificamos Variables
# X <- Calificacion alumnos que van a clase
# Y <- Calificacion alumnos que no van a clase
n <- 8
m <- 8
#H0 <- mu_x = mu_y
#H1 <- mu_x =no mu_y
# como n,m < 30 e independientes nos preguntamos si las varianzas son iguales
#H0 <- var_x = var_y  o lo que es lo mismo, var_x/var_y = 1
#H1 <- var_x =no var_y o lo que es lo mismo, var_x/var_y =no 1
x <- c(2.5, 1.5, 7,4,8.2,6.8,9.5,8)
y <- c(0.75,1.5,3,1,4,4.5,6.5,5)

var.test(x,y, ratio = 1, alternative = "two.sided")
#vemos que p-value > alpha , por tanto no podemos rechazar H0 y suponemos que ambas varianzas son iguales
#ahora hacemos el contraste que queriamos inicialmente
t.test(x,y, alternative = "two.sided", paired = FALSE, var.equal = TRUE)
#vemos que p-value > alpha (0.05), por tanto no podemos rechazar H0, y podemos suponer las medias iguales

## Apartado B
#Podemos tambien plantearnos los unilaterales de que una sea mayor o menor que la otra
t.test(x,y, alternative = "greater", paired = FALSE, var.equal = TRUE)
#Vemos que p-value < alpha por tanto podemos rechazar H0, aceptar H1 y diriamos que la media de los que van a clase es mayor que la media de los que no van
t.test(x,y, alternative = "less", paired = FALSE, var.equal = TRUE)
#vemos que p-value > alpha por tanto no podemos rechazar H0.

#######
#Ejercicio 2
encender <- c(169.7, 168.5,165.9,177.8,179.6,168.9,169.2,167.9,181.8,163.3)
horas <- c(168.2,165.5,164.4,175.7,176.6,166.1,167.1,166.3,179.7,161.5)
diferencia <- encender - horas
t.test(encender, horas, alternative = "two.sided", mu = 0, paired = TRUE)
t.test(diferencia, alternative = "two.sided", mu = 0)
#por tanto rechazamos H0 y podemos decir que existen diferencias entre las medias y que el tiempo de ejecución se reduce cuando el ordenador lleva dos horas de funcionamiento

#######
#Ejercicio 3
ciudad1 <- c(5.9,6.1,6.3,6.1,6,6.2,5.7,6.3,6.6)
ciudad2 <- c(6.4,6.3,6.5,6.1,5,5.5,4.7,5.1,2.8,1.8,1.5)
var.test(ciudad1,ciudad2, ratio = 1, alternative = "two.sided" )
#por tanto rechazamos H0, aceptamos H1 diriamos que las varianzas son distintas
t.test(ciudad1,ciudad2, paired = FALSE, var.equal = FALSE, alternative = "two.sided")
#lo hacemos que alpha es 0.1 como alpha > p-value rechazamos H0 y decimos que hay diferencia entre los salarios.

######
#Ejercicio 4 
n <- 114 #han votado X
m <- 123 #no han votado Y
x <- rnorm(114, mean = 2.71, sd = 0.64)
X <- 2.71 + (x - mean(x))*0.64/sd(x)
y <- rnorm(123, mean = 2.79, sd = 0.56)
Y <- 2.79 + (y - mean(y))*0.56/sd(y)
t.test(X,Y, alternative = "less", paired = FALSE, var.equal = FALSE, )
pvalor <- pnorm(-1.0201)
#como p-value > alpha, no podemos rechazar H0, supondríamos que son iguales
z.test(X,Y, alternative = "less", sigma.x = 0.64, sigma.y = 0.56, paired = TRUE)

######
#Ejercicio 5
x <- c(0.6,1.2,0.9,1.9,2.0,0.6,0.9,2.0,0.8,1.0)
y <- c(0.4,1.3,1.1,2.1,1.9,0.5,1.1,1.7,0.8,1.1)
#H0 mu_x - mu_y = 0
#h1 mu_x - mu_y =! 0
t.test(x,y, mu = 0, alternative = "two.sided", paired = TRUE)
#vemos que p-value (0.8638) > (0.05) alpha, por tanto no podemos rechazar H0 y diremos que la media es igual en ambos departamentos

######
#Ejercicio 6
#bajo mi punto de vista, no podemos suponerlos independientes
n1 <- c(14,12,15,15,11,16,17,12,14,13,18,13,18,15,16,11)
n2 <- c(20,22,18,18,19,15,18,15,22,18,19,15,21,22,18,16)
#Tenemos dos muestra independientes, que no son normales y con n y m < 30, por tanto vamos a mirar si podemos suponer las varianzas iguales.
var.test(n1,n2, alternative="two.sided", ratio = 1)
#vemos que p-value > alpha por tanto podemos suponer que ambas varianzas son iguales
t.test(n1,n2, alternative = "two.sided", var.equal = TRUE, paired = FALSE)
#Vemos que alpha > p-value, por tanto rechazamos H0, aceptamos H1 y podemos afirmar que el ruido si afecta en la habilidad de las
#personas para realizar la actividad, concretamente hace que tarden más en realizar la actividad, vamos a confirmarlos con el siguiente
#contraste
t.test(n1,n2, alternative = "greater", var.equal = TRUE, paired = FALSE)

########
#clase excel
#se desea contrastar si el tiempo en realizar una tarea por cierto trabajadores disminuye con el tiempo.
#Se toma una muestra de tamaño n = 20 trabajadores y se miden sus tiempos
# X <- tiempo empleado antes" N(mu1, sigma1)
# Y <- tiempo empleado despues" N(mu2, sigma2)
#H0 <- mu1 <= mu2

#como no son idependientes, realmente el contraste es D <- X-Y
# mu_d = 0