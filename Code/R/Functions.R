#Bibliotecas utilizadas#
library(combinat)

definePatterns<-function(dimension,option=0){
  lista = permn(dimension) 
  fat = factorial(dimension)
  symbol = matrix(nrow = fat,ncol = dimension)
  for(i in 1:fat){
    v=lista[i]
    symbol[i,]=unlist(v)
  }
  if(option)
    return(symbol[option,])
  else
    return(symbol)
}

#Option = 0 -> return p_patterns
#Option = 1 -> return n_symbols
#Option = 2 -> return elements
#Option = 3 -> return index
formationPattern<-function(series,dimension,delay,option=0){
  n_symbols = 1
  i = first = 1
  n = length(series)
  elements = p_patterns = index = matrix(nrow=n,ncol=dimension)
  orde = matrix(nrow = 1, ncol = dimension)    
  while(i <= n){      
    for(j in 1:dimension){
      elements[n_symbols,j]=series[i]
      index[n_symbols,j]=i
      i = i + 1
      if(i >= n+1){
        break
      }
    }      
    if(j==dimension && i<= n+1){ 
      orde[1,]=order(elements[n_symbols,])
      #busca linear
      for(a in 1:dimension){
        for(b in 1:dimension){
          if(elements[n_symbols,orde[1,a]] == elements[n_symbols,b]){
            p_patterns[n_symbols,b]=a
          }
        }
      }
      i=(first+delay)
      first=i
      n_symbols=n_symbols+1
    }      
    else{
      break
    }
  }
  aux = na.omit(elements)
  n_symbols = dim(aux)[1]
  if(option == 0){
    p_patterns = na.omit(p_patterns)
    return ( p_patterns[1:dim(p_patterns)[1],])
  }
  else if(option == 1){
    return (n_symbols)
  }
  else if(option == 2){
    return (aux)
  }
  else if(option == 3){
    return(index)
  }
}


distribution<-function(serie,dimension,delay,option=1){  
  fat = factorial(dimension)
  f_absolute = probability = rep(0,fat)
  initial = 1
  end = length(serie)
  p_patterns <- formationPattern(serie,dimension,delay)
  p_patterns <- newPatterns(p_patterns)
  n_symbols <- dim(p_patterns)[1]
  symbols <- definePatterns(dimension)
  for(i in 1:fat){
    for(j in 1:n_symbols){
      if(all(p_patterns[j,] == symbols[i,])){ 
        f_absolute[i]=f_absolute[i]+1
      }
    }
  }
  probability = f_absolute/n_symbols
  if(!option){
    return(f_absolute)
  }else{
    return(probability)
  }
}





