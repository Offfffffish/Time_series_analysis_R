library(combinat)

definePatterns<-function(dimension){
  symbol = matrix(unlist(permn(dimension)),nrow = factorial(dimension),ncol = dimension,byrow = TRUE)
  symbol = symbol - 1
  symbol
}

formationPattern<-function(serie,dimension,delay,option){
  n_symbols = i = 1
  n = length(serie)
  p_patterns = elements = matrix(nrow=n,ncol=dimension)
  index = c(0:(dimension-1)) 
  while(i <= n){    
    first = i
    if((i+dimension-1)<=n){
      elements[n_symbols,] = serie[i:(i+dimension-1)]
      p_patterns[n_symbols,] = index[order(elements[n_symbols,])]
      i = first + delay
      n_symbols = n_symbols + 1
    }else break
  }
  if(option == 0){
    p_patterns = na.omit(p_patterns)
    p_patterns[1:dim(p_patterns)[1],]
  }else{
    elements = na.omit(elements)
    elements[1:dim(elements)[1],]    
  }
}


distribution<-function(serie,dimension,delay){  
  fat = factorial(dimension)
  probability = rep(0,fat)
  p_patterns <- formationPattern(serie,dimension,delay,0)
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





