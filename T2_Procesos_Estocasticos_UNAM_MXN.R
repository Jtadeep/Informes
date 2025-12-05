# Tarea 2
# Procesos Estocásticos
# Juan Pablo Zamora Alarcón
# Sistema Bonus-Malus

#1. Proponer una distribución inicial.


p <- 0.5 # Probabilidad de que no ocurra siniestro
# Creamos la matriz de transición en función de p:
entries =   c(p,0,0,0,0,1-p,
            + p,0,0,0,0,1-p,
            + 0,p,0,0,0,1-p,
            + 0,0,p,0,0,1-p,
            + 0,0,0,p,0,1-p,
            + 0,0,0,0,p,1-p)
TP = matrix(entries,nrow = 6,byrow=TRUE)
TP
# Calculamos la distribución estacionaria
# Calculamos los valores propios de la matriz transpuesta estacionaria
# ya que el vector propio izquierdo coincide con el vector propio derecho de la transpuesta
# se divide entre la suma de los elementos, para que las entradas sumen 1

eigen(t(TP))$vectors[,1]/sum(eigen(t(TP))$vectors[,1])

# En este caso, tenemos que el 3% de los asegurados estarán sin siniestros el primer año, asi 
# hasta que el 50% de los asegurados estarán 6 años sin siniestros.

#2. Proponer un vector de primas.

#Ahora definirémos un vector de descuento de la prima por el buen comportamiento
v=c(1,0.9,0.85,0.80,0.75,0.7)

#3. Calcular la prima promedio que se paga.

#Calculamos la función potencia para la matriz TP
potencia_TP <- function(n){
  if(n==1) return(TP)
  if(n==2) return(TP%*%TP)
  if(n>2) return(TP%*%potencia_TP(n-1))
}
potencia_TP(3) # ejemplo potencia de la matriz TP en el año 3

# Ahora calcularemos la prima promedio para el año n
p=numeric(0)
prima_0<-50 # Prima inicial
prima<-function(n){
  for(j in 1:length(v)){
    p[j]=mean(potencia_TP(n)[,j])
    } 
  + prima_0*sum(p*v)
}

# Si calculamos las primas para 10 años
primas_10 <- numeric(0)
for (n in 1:10) {
  primas_10[n]=prima(n)
}
primas_10






