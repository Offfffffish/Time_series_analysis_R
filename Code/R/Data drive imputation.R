    library(combinat)
    library(gtools)

    dataDriven<-function(serie, dimension, delay){
      n_symbols = i = 1
      symbols <- definePatterns(dimension)
      fat = factorial(dimension)
      n = length(serie)
      patterns = elements = matrix(nrow=n,ncol=dimension)
      index = c(0:(dimension-1)) 
      p1=we = rep(0,fat)
      while(i <= n){ 
        first = i
        if((i+dimension-1)<=n){
          elements[n_symbols,] = serie[i:(i+dimension-1)]
          patterns[n_symbols,] = index[order(elements[n_symbols,])]
          aux = duplicated(elements[n_symbols,])
          isDuplicated = length(aux[aux==TRUE])
          if(isDuplicated==0){
            for(j in 1:fat){
              if(all(patterns[n_symbols,] == symbols[j,])){
                p1[j]=p1[j]+1 
                we[j]=we[j]+1
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
        if(isDuplicated != 0){
          aux = duplicated(elements[i,order(elements[i,])])
          init = which(aux==TRUE)[1] - 1
          end = init + isDuplicated
          myresult = mypermute(patterns[i,],init,end)
          myp = rep(0,dim(myresult)[1])
          mysum = 0
          for(w in 1:dim(myresult)[1]){
            for(j in 1:fat){
              if(all(myresult[w,] == symbols[j,])){ 
                mysum = mysum + p1[j]
                myp[w] = j
                break
              }
            }   
          }
          for(w in 1:dim(myresult)[1]){
            we[myp[w]] = we[myp[w]] + p1[myp[w]]/mysum
          }
        }
        
      }
      print(we)
      print(n_symbols-1)
      we = we/(n_symbols-1)
      we
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

