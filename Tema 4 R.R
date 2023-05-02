T1 <- c(8, 11, 10)
T2 <- c(3,2,1,3,2)
T3 <- c(3,4,5,4)

datos <- c(T1,T2,T3)
boxplot(datos)

grupos <- c(rep(1,length(T1)),
            rep(2,length(T2)),
            rep(3,length(T3)))
grupos <- as.factor(grupos)

boxplot(datos ~ grupos)
boxplot(T1,T2,T3)
hist(T1)
density(T1)
plot(density(T1))

plot(density(T1), xlim = c(0, 15), ylim = c(0, 1))
lines(density(T2))
lines(density(T3))

anova <- aov(datos ~grupos)
anova
summary(anova)
TukeyHSD(anova, conf.level = 0.99)
plot(TukeyHSD(anova, conf.level = 0.99))

####
#####
#Ejercicio 2
lab1 <- c(2.3,4.1,4.9,2.5,3.1,3.7)
lab2 <- c(6.5, 4.0, 4.2, 6.3, 4.4)
lab3 <- c(1.7, 2.7, 4.1, 1.6, 4.1, 2.8)
lab4 <- c( 2.1, 3.8, 4.8, 2.8, 4.8, 3.7, 4.2)
lab5 <- c(8.5, 5.5, 6.1, 8.2)

datos <- c(lab1,lab2,lab3,lab4,lab5)
grupos <- c(rep(1, length(lab1)),
            rep(2, length(lab2)),
            rep(3,length(lab3)),
            rep(4,length(lab4)),
            rep(5,length(lab5)))
grupos <- as.factor(grupos)
anova <- aov(datos ~grupos)
boxplot(datos~grupos)
plot(density(lab1), xlim = c(0, 15), ylim = c(0, 1), col= "purple")
lines(density(lab2), col = "green")
lines(density(lab3), col = "blue")
lines(density(lab4), col = "pink")
lines(density(lab5), col = "red")
summary(anova)

# Rechazamos H0, aceptamos H1, hay alguna media distitna

TukeyHSD(anova)
plot(TukeyHSD(anova))


2*pbinom(q = 6, size = 10,prob = 0.5,lower.tail = F)
2*pnorm(5/3, lower.tail = FALSE)
2*pnorm(83.81,lower.tail = TRUE)

datos <- c(6,12,19,23,3,11,15,20,7,8,8,9,20,21,23,35,45,50,55)
boxplot(datos)
