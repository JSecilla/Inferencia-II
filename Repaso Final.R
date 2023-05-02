datos <- datosRepasoGlobal1

#1-------------------------------------------------------------------------------
#Ejercicio 1
library(nortest)
#H0 <- X ~ N(mean, cuasi)

lillie.test(datos$PesoAntes)
#Vemos que no podemos rechazar normalidad

#hacemos contraste paramétrico

#H0 <- Mu = 85
t.test(x = datos$PesoAntes,mu = 85,alternative = "two.sided")

#No tengo evidencias para rechazar que el peso medio de los que entran a hacer dieta

#2-------------------------------------------------------------------------------
#Apartado 2

#H0 <- X ~ N(mean, cuasi)
lillie.test(datos$PesoDespues)
#No podemos rechazar Normalidad

#H0 <- Mu = 65
t.test(x = datos$PesoDespues,mu = 65,alternative = "two.sided")
#p-valor ~ 0 , rechazo H0, por lo que el peso después de la diesta no es 65

mean(datos$PesoDespues)
#Vemos que parece que es mayor de 65
t.test(x = datos$PesoDespues,mu = 65,alternative = "greater")
#p-valor ~ 0 , rechazo H0, por lo que el peso después de la diesta es mayor a 65

#3-------------------------------------------------------------------------------
#Apartado 3

#Creamos la variable diferencia, además ya sabemos que va a ser normal
D <- datos$PesoAntes - datos$PesoDespues
t.test(x = D,mu = 0,alternative = "greater")
#o
t.test(datos$PesoAntes,datos$PesoDespues,alternative = "greater",paired = TRUE)
#Aceptamos H1, por lo que diremos que la dieta tiene efecto en las personas
#4------------------------------------------------------------------------------
#filtramos los datos
infra <- datos[datos$categoriaIMC == "Infrapeso",]
normo <- datos[datos$categoriaIMC == "Normopeso",]
sobre <- datos[datos$categoriaIMC == "Sobrepeso",]

estatura <- c(infra$Estatura,normo$Estatura,sobre$Estatura)
grupos <- as.factor(c(rep(1,length(infra$Estatura)),
            rep(2,length(normo$Estatura)),
            rep(3,length(sobre$Estatura))))
anova = aov(estatura ~ grupos)
summary(anova)

boxplot(estatura ~grupos)

#Podriamos haberlo hecho más rapido
anova2 <- aov(datos$Estatura ~ datos$categoriaIMC)
summary(anova2)

#Vamos a ver el Test de Tukey para vewr cuales son iguales entre si

TukeyHSD(anova)
