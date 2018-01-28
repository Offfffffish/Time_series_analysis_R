library(combinat)

definePatterns<-function(dimension){
  symbol = matrix(unlist(permn(dimension)),nrow = factorial(dimension),ncol = dimension,byrow = TRUE)
  symbol = symbol - 1
  symbol
}

formationPattern<-function(series,dimension,delay){
  n_symbols = i = 1
  n = length(series)
  p_patterns = matrix(nrow=n,ncol=dimension)
  index = c(0:(dimension-1)) 
  while(i <= n){    
    first = i
    if((i+dimension-1)<=n){
      p_patterns[n_symbols,] = index[order(series[i:(i+dimension-1)])]
      i = first + delay
      n_symbols = n_symbols + 1
    }else break
  }
  p_patterns = na.omit(p_patterns)
  p_patterns[1:dim(p_patterns)[1],]
}


distribution<-function(serie,dimension,delay){  
  fat = factorial(dimension)
  probability = rep(0,fat)
  p_patterns <- formationPattern(serie,dimension,delay)
  n_symbols <- dim(p_patterns)[1]
  symbols <- definePatterns(dimension)
  for(i in 1:fat){
    for(j in 1:n_symbols){
      if(all(p_patterns[j,] == symbols[i,])){ 
        probability[i]=probability[i]+1
      }
    }
  }
  probability = probability/n_symbols
  probability
}





