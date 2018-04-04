library(combinat)
library(gtools)

dataDriven2<-function(serie, dimension, delay){
  symbols <- definePatterns(dimension)
  fat = factorial(dimension)
  n = length(serie)
  pattern = elements = matrix(nrow=n,ncol=dimension)
  index = c(0:(dimension-1)) 
  p1 = we = we1 = rep(0,fat)
  nore = 0

  elements = formationPattern(serie, dimension, delay, 1)
  pattern = formationPattern(serie, dimension, delay, 0)

  p1 = we = completeTies(elements, pattern,0)
  re = completeTies(elements, pattern, 1)
  
  n_symbols = dim(elements)[1]
  
  for(i in 1:(n_symbols)){
    aux = duplicated(elements[i,])
    isDuplicated = length(aux[aux==TRUE])
    if(isDuplicated != 0){
      aux = duplicated(elements[i,order(elements[i,])])
      init = which(aux==TRUE)[1] - 1
      end = init + isDuplicated
      myresult = mypermute(pattern[i,],init,end)
      myp = rep(0,dim(myresult)[1])
      mysum = 0
      teste = 0
      for(w in 1:dim(myresult)[1]){
        for(j in 1:fat){
          if(all(myresult[w,] == symbols[j,])){ 
            myp[w] = j
            we1[w] = we1[w] + 1
            break
          }
        }   
      }
      mysum = sum(p1[myp])
      for(w in 1:dim(myresult)[1]){
        if(mysum != 0){
          we[myp[w]] = we[myp[w]] + p1[myp[w]]/mysum
          teste = teste + p1[myp[w]]/mysum
        }
      }
      if(teste != 1){
        print("Falta de precisão")
      }else{
        nore = nore + 1
      }
    }
  }
  we = we/(re + nore)  
  we
}

completeTies <- function(elements,pattern,option){
  fat = factorial(dim(elements)[2])
  number = dim(elements)[1]
  p1 = rep(0,fat)
  symbols <- definePatterns(dim(elements)[2])
  re = 0
  for(i in 1:number){
    aux = duplicated(elements[i,])
    isDuplicated = length(aux[aux==TRUE])
    if(isDuplicated==0){
      re = re + 1
      for(j in 1:fat){
        if(all(pattern[i,] == symbols[j,])){
          p1[j]=p1[j]+1 
        }
      }
    }
  }
  if(option == 0){
    return(p1)
  }else{
    return(re)
  }
}

mypermute<-function(vector, init, end){
  a = matrix(unlist(permn(vector[init:end])),nrow = factorial(end-init+1),ncol = end-init+1,byrow = TRUE)
  number = dim(a)[1]
  result = matrix(nrow = number,ncol = length(vector))
  for(i in 1:number){
    if(1 < init){  
      result[i,1:(init-1)] = vector[1:(init-1)]
    }
    result[i,init:end] = a[i,]
    if(end < length(vector)){
      result[i,(end+1):length(vector)] = vector[(end+1):length(vector)]
    }
  }
  result
}
