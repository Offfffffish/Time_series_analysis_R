#Trata padrões com elemento repetidos
newPatterns<-function(patterns){
  n_patterns = dim(patterns)[1]
  dimension = dim(patterns)[2]
  for(i in 1: n_patterns){
    aux = duplicated(patterns[i,])
    occourences = rep(0,dimension)
    for(j in 1:dimension){
      if(aux[j]){
        occourences[patterns[i,j]] = occourences[patterns[i,j]] + 1
        patterns[i,j] = patterns[i,j] - occourences[patterns[i,j]]
      }
    }
  }
  patterns
}