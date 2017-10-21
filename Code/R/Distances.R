euclidian_distance<-function(probability){
  c = rep(1/length(probability),length(probability))
  distance = sum((probability-c)^2)
  return(sqrt(distance))
}

euclidian_quadratica_distance<-function(probability){
  c = rep(1/length(probability),length(probability))
  distance = sum((probability-c)^2)
  return(distance)
}

manhattan_distance<-function(probability){
  c = rep(1/length(probability),length(probability))
  distance = sum(abs(probability-c))
  return(distance)
}

chebyshev_distance<-function(probability){
  c = rep(1/length(probability),length(probability))
  L = abs(probability - c)
  return(max(L))
}

kullback_leibler_divergence<-function(probability){
  c = rep(1/length(probability),length(probability))
  distance <- probability * log(probability/c)
  distance[is.nan(distance)] <- 0
  return(sum(distance))
}

kullbach_aux<-function(p,q){
  distance <- p * log(p/q)
  distance[is.nan(distance)||is.infinite(distance)]<-0
  return(sum(distance))
}

hellinger_Distance<-function(probability){
  c = rep(1/length(probability),length(probability))
  distance = sum((sqrt(probability)-sqrt(c))^2)*0.5
  return(sqrt(distance))
}

jensenDivergence<-function(p){
  q = rep(1/length(p),length(p))
  s_p = shannonEntropy(p)
  s_q = shannonEntropy(q)
  s_pq = shannonEntropy((p+q)/2)
  divergence = sum( s_pq - (s_p/2) - (s_q/2))
  return(divergence)
}

constant <- function(p){
  v1 = (0.5)/length(p)
  aux1 = (0.5 + v1) * log(0.5 + v1)
  aux2 = (length(p) - 1) * v1 * log(v1)
  aux3 = (1 - 0.5) * log(length(p))
  q03 = -1/(aux1 + aux2 + aux3)
  return(q03)
}

Ccomplexity<-function(p){
  c <- jensenDivergence(p) * constant(p) *
    shannonEntropyNormalized(p)
  return(c)
}

wootters_distance<-function(probability,q){
  c = rep(1/length(probability),length(probability))
  dis = sum(sqrt(probability*c))
  dis = acos(dis)
  return(dis)
}

complexityF<-function(probability){
  entropy = ShannonAux(probability)
  qInitial = rep(1/length(probability),length(probability))
  qInitial[1] = 1
  desequilibrium = jensenDivergence(qInitial) *
    jensenDivergence(probability)
  comp = entropy * desequilibrium
  return(comp)
}