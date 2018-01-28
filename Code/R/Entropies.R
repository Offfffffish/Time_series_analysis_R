shannonEntropy <- function(probability){
  h <- probability * log(probability)
  h[is.nan(h)] <- 0
  return(-sum(h))
}

shannonEntropyNormalized <- function(probability){
  return(shannonEntropy(probability)/log(length(probability)))
}

tsallisEntropy <- function(probability,q){  
  entropy = sum(probability^q)
  entropy = (1 - entropy)/(q - 1)
  entropy
}

tsallisEntropyNormalized <- function(probability,q){  
  ent_max = (1 - (length(probability)^(1 - q)))/(q - 1)
  return(tsallisEntropy(probability,q)/ent_max)
}

renyiEntropy <- function(probability,q){
  entropy = sum(probability^q)
  entropy = log(entropy)
  entropy = entropy/(1 - q)
  entropy
}

renyiEntropyNormalized <- function(probability,q){ 
  return(renyiEntropy(probability,q)/log(length(probability)))
}
  
  
  
  
