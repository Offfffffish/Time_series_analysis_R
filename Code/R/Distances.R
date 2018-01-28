euclidian_distance<-function(probability){
  return(sqrt(squared_euclidian_distance(probability)))
}

squared_euclidian_distance<-function(probability){
  cc = rep(1/length(probability),length(probability))
  distance = sum((probability-cc)^2)
  return(distance)
}

manhattan_distance<-function(probability){
  cc = rep(1/length(probability),length(probability))
  distance = sum(abs(probability-cc))
  return(distance)
}

chebyshev_distance<-function(probability){
  cc = rep(1/length(probability),length(probability))
  distance = abs(probability - cc)
  return(max(distance))
}

kullback_leibler_divergence<-function(probability){
  cc = rep(1/length(probability),length(probability))
  distance = probability * log(probability/cc)
  distance[is.nan(distance)||is.infinite(distance)] <- 0
  return(sum(distance))
}

hellinger_distance<-function(probability){
  cc = rep(1/length(probability),length(probability))
  distance = sum((sqrt(probability)-sqrt(cc))^2)*0.5
  return(sqrt(distance))
}

jensen_divergence<-function(probability){
  cc = rep(1/length(probability),length(probability))
  s_p = shannonEntropy(probability)
  s_q = shannonEntropy(cc)
  s_pq = shannonEntropy((probability+cc)/2)
  divergence = sum(s_pq - (s_p/2) - (s_q/2))
  return(divergence)
}

constant <- function(p){
  k = (0.5)/length(p)
  a1 = (0.5 + k) * log(0.5 + k)
  a2 = (length(p) - 1) * k * log(k)
  a3 = (1 - 0.5) * log(length(p))
  b = -1/(a1 + a2 + a3)
  return(b)
}

Ccomplexity<-function(p){
  cc <- jensenDivergence(p) * constant(p) * shannonEntropyNormalized(p)
  return(cc)
}

wootters_distance<-function(probability,q){
  cc = rep(1/length(probability),length(probability))
  distance = sum(sqrt(probability*cc))
  distance = acos(distance)
  return(distance)
}

bhattacharyya_distance<-function(probability){
  cc = rep(1/length(probability),length(probability))
  distance = sum(sqrt(probability*cc))
  distance = -log(distance)
  distance
}

harmonic_mean<-function(probability){
  cc = rep(1/length(probability),length(probability))
  harmonic = sum((probability*cc)/(probability+cc))
  harmonic[is.nan(harmonic)||is.infinite(harmonic)] <- 0
  return(2*harmonic)
}