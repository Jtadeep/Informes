# Tarea 3
# Procesos Estocásticos
# Juan Pablo Zamora Alarcón
# Proceso de Poisson
# Ejemplo 3.1.2

#Probabilidad de categorias
p <- c(1/9,1/18,4/18,1/18,10/18)
p1<-1/9
p2<-1/18
p3<-4/18
p4<-1/18
p5<-10/18
# Parámetro de intensidad diaria
lambda<-3
# P1:  Cual es la probabilidad que en una semana se presenten 5 reclamaciones de la categora 5?
# Seria calcular P[5N7 = 5]
x <- p5*lambda*7
dpois(5,x)
# P2:   Cual es el numero de reclamaciones esperadas en un mes de cada categora?
# Seria calcular E[1N30],...,E[5N30] 
E1<-lambda*p1*30
E2<-lambda*p2*30
E3<-lambda*p3*30
E4<-lambda*p4*30
E5<-lambda*p5*30
N_E<-c(E1,E2,E3,E4,E5)
N_E
# P3:  Cual es el tiempo esperado hasta la octava reclamacion de la categora 1?
# Seria calcular E[1S8]
E8<-8/(lambda*p1)
E8
# P4:  Si en una semana se presentan 3 reclamaciones, cual es la probabilidad que todas sean del mismo tipo?
# Seria calcular P[1N7 = 3 o 2N7 = 3 o ... o 5N7 = 3 | N7 = 3]
# P[1N7 = 3 o 2N7 = 3 o ... o 5N7 = 3 | N7 = 3] = Suma P[iN7 = 3 | N7 = 3], i=1...5.
# Dado que se pregunta en que las 3 reclamaciones sean de la misma categoria
# donde P[iN7 = 3 | N7 = 3]=pi*pi*pi=pi^3
Probabilidad_3_Iguales <- sum(p^3)
Probabilidad_3_Iguales



