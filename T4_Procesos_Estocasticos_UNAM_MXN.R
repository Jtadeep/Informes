# Tarea 4
# Procesos Estocásticos
# Juan Pablo Zamora Alarcón
# Proceso de Poisson compuesto

# Suponer es que las Yi distribuyen expo(alpha)
# Proponer el parametro u > 0
# Proponer C > 0
# Proponer lambda >0 
# Proponer alpha >0 parámetro Yi
# Proponer T >0 tiempo de cobertura

# lamba/alpha < C 

# Estimar la probabilidad de ruina eventual

cramlund <- function(u0, cte, lambda, alpha, T){
  tiempo <- 0
  Ct <- u0
  severidad <- 0
  ruina <- 0
  repeat{
    p0 <- c(tiempo, Ct)
    tn <- rexp(1,lambda)
    tiempo <- tiempo + tn
    Ct <- Ct + cte*tn
    Ct <- Ct - rexp(1,alpha)
    if(Ct < 0){
      severidad <- Ct
      ruina <- 1
      break
    }
    if(tiempo > T) break
  }
  return (c(ruina, severidad))
}

u0=20
c=1
lambda=1
alpha=2
# Se cumple condicion de c>lambda/alpha
T=200
#número simulaciones
n<-1000
sim<-replicate(n,cramlund(u0,c, lambda, alpha, T)[1])
# Probailidad empírica
mean(sim)
# La probabilidad de ruina con capital inicial u0:
prob_ruina_analitica<-lambda/(c*alpha)*exp(-alpha*(1-lambda/(c*alpha))*u0)
prob_ruina_analitica

