# Tarea 1 
# Procesos Estocásticos
# Juan Pablo Zamora Alarcón
# Estimar el tiempo de duración del juego y la probabilidad de ruina.

cap_ruina_jugador <-function(k,p,n){
  #evolución del capital
  ev=k
  #generan va unif en (0,1)
  U=runif(n)
  # contador duracion
  duracion=0
  for(i in 1:n){
    if(U[i]<=p){
      Ind=1
    }
    else{
      Ind=0
    }
    Y=2*Ind-1
    ev=c(ev,tail(ev,1)+Y)
    if(tail(ev,1)==0)break
    if(tail(ev,1)==2*k)break
    duracion=duracion+1
  }
  resultado <- ifelse(tail(ev,1)==0,"ruina","exito")
  return(list(trayectoria=ev,duracion=duracion,resultado=resultado))
}

# capital inicial
k=2
#probabilidad de cara
p=1/3
#número de lanzamientos de la moneda
n=100
#ejecutando funcion
cap=cap_ruina_jugador(k,p,n)
cap

# simular
n=10
mean(replicate(1000,cap_ruina_jugador(k,p,n)$duracion))
mean(replicate(1000,cap_ruina_jugador(k,p,n)$resultado)=="ruina")
n=50
mean(replicate(1000,cap_ruina_jugador(k,p,n)$duracion))
mean(replicate(1000,cap_ruina_jugador(k,p,n)$resultado)=="ruina")
n=100
mean(replicate(1000,cap_ruina_jugador(k,p,n)$duracion))
mean(replicate(1000,cap_ruina_jugador(k,p,n)$resultado)=="ruina")
n=1000
mean(replicate(1000,cap_ruina_jugador(k,p,n)$duracion))
mean(replicate(1000,cap_ruina_jugador(k,p,n)$resultado)=="ruina")
n=10000
mean(replicate(1000,cap_ruina_jugador(k,p,n)$duracion))
mean(replicate(1000,cap_ruina_jugador(k,p,n)$resultado)=="ruina")