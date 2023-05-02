########
#########
#Ejercicios Tema 7
#--------------------------------
#Ejercicio 1
#/////////////////////////////////////
datos <- matrix(c(400,100,500,600,400,500), nrow = 2,ncol = 3,byrow = TRUE)

chisq.test(datos)

#////////////////////////////////////////
#Ejercicio 2
datos <- matrix(c(4,96,6,94),nrow = 2,ncol = 2,byrow = TRUE)
chisq.test(datos)
 #Aqui el estadisitico sale distitnto al ser la tabla 2*2 


#Ejercicio 3
datos <- matrix(c(8,46,20,126,5,35,8,152),nrow = 2,ncol = 4,byrow = TRUE)
chisq.test(datos)

#Ejercicio 4
ninos <- c(86, 69, 72, 65, 113, 65, 118, 45, 141, 41, 50 ,104)
ninas <- c(55, 40, 22, 58, 16, 7, 9, 16, 26, 36, 20, 15)
plot(ecdf(ninos), xlim = c(min(ninos,ninas),max(ninos,ninas)))
lines(ecdf(ninas), col = "red") #Esto lo hacemos en KS
ks.test(ninos,ninas)

#apartado B
wilcox.test(ninos,ninas)  #Esos dos los hacemos ya que la n y m son pequeñas
wilcox.exact(ninos,ninas)

#si estos fuesen grandes lo hariamos de otra formas
wilcox_test(c(ninos,ninas)~ as.factor(c(rep(1,length(ninos)),
                                      rep(2,length(ninas)))))


#Ejercicio 5
A <- c(11.5, 10.8, 11.6, 9.4, 12.4, 11.4, 12.2, 11, 10.6, 10.8)
B <- c(11.8, 12.6 ,12.2, 12.5, 11.7, 12.1 ,10.4, 12.6)
plot(ecdf(A), xlim = c(min(A,B),max(A,B)))
lines(ecdf(B), col = "red")

ks.test(A,B) #Como p-valor es < que 0.1 y 0.05 podemos rechazar H0
             # para alfa 0.01 no puedo rechazar H0


#Apartado B
#como los tamaños muestrales son pequeños yu no tenemos valores repetidos, nos vamos a test exactos
wilcox.test(A,B) #Mismas conclusiones que antes
wilcox.exact(A,B)

##############################
#wilcox.test lo usamos si n,m < 50 y no hay valores repetidos <- exacto
# si n,m > 50, y/o hay valores repetidos nos va a dar una aproximacion normal

# wilcox.exact (exactRankTests) va a hacer un exacto condicional a los datos,
#incluso para n >> (algoritmo)

#wilcox_test (coin) es aproximado, es bastante bueno para n,m >>

#entonces si n,m pequeños, usariamos uno de los dos primerios
#si n,m grande, usaremos o el primero o el tercero
