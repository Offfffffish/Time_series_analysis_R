library(combinat)
library(gtools)

dataDriven<-function(serie, dimension, delay){
  n_symbols = i = 1
  symbols <- definePatterns(dimension)
  fat = factorial(dimension)
  n = length(serie)
  patterns = elements = matrix(nrow=n,ncol=dimension)
  index = c(0:(dimension-1)) 
  p1=we = p2= rep(0,fat)
  while(i <= n){ 
    first = i
    if((i+dimension-1)<=n){
      elements[n_symbols,] = serie[i:(i+dimension-1)]
      aux = duplicated(elements[n_symbols,])
      isDuplicated = length(aux[aux==TRUE])
      patterns[n_symbols,] = index[order(elements[n_symbols,])]
      if(isDuplicated==0){
        for(j in 1:fat){
          if(all(patterns[n_symbols,] == symbols[j,])){
            p1[j]=p1[j]+1
          }
        }
      }
      n_symbols = n_symbols + 1
      i = first + delay
    }else break
  }
  for(i in 1:(n_symbols-1)){
    aux = duplicated(elements[i,])
    isDuplicated = length(aux[aux==TRUE])
    if(isDuplicated == 0){ #Cortando os símbolos com valores repetidos
      for(j in 1:fat){
        if(all(patterns[i,] == symbols[j,])){ 
          we[j]=we[j]+1
        }
      }
    }else{
      aux = duplicated(elements[i,order(elements[i,])])
      init = which(aux==TRUE)[1] - 1
      end = init + isDuplicated
      myresult = mypermute(patterns[i,],init,end)
      for(j in 1:fat){
        for(w in 1:dim(myresult)[1]){
          if(all(myresult[w,] == symbols[j,])){ 
            we[j] = we[j] + p1[j]/(sum(p1[1:dim(myresult)[1]]))
          }
        }
      }
    }
    
  }
  p2 = we/(n_symbols-1)
  p2
}

mypermute<-function(vector, init, end){
  a = matrix(unlist(permn(vector[init:end])),nrow = factorial(end-init+1),ncol = end-init+1,byrow = TRUE)
  number = dim(a)[1]
  result = matrix(nrow = number,ncol = length(vector))
  for(i in 1:number){
    if(1 < init){  result[i,1:(init-1)] = vector[1:(init-1)]}
    result[i,init:end] = a[i,]
    if(end < length(vector)){ result[i,(end+1):length(vector)] = vector[(end+1):length(vector)]}
  }
  result
}
