library(ggplot2)
require(ggthemes)


readingMPR<-function(dimension,option=0){
  if(dimension == 3){ 
    continua = "continuaN6.txt"
    trozo = "trozosN6.txt"
  }
  if(dimension == 4){ 
    continua = "continuaN24.txt"
    trozo = "trozosN24.txt"
  }
  if(dimension == 5){ 
    continua = "continuaN120.txt"
    trozo = "trozosN120.txt"
  }
  if(dimension == 6){ 
    continua = "continuaN720.txt"
    trozo = "trozosN720.txt"
  }
  curva1x = Read_txt(continua,1) 
  if(option==1) return(curva1x)
  curva1y = Read_txt(continua,2)
  if(option==2) return(curva1y)
  curva2x = Read_txt(trozo,1)
  if(option==3) return(curva2x)
  curva2y = Read_txt(trozo,2)
  if(option==4) return(curva2y)
}

# Partition indicates the number of parts in which we divide the series
partitionMPR<-function(serie,dimension,delay,partition){
  complexity = entropy = rep(0,partition)
  div = floor(length(serie)/partition)
  if(partition != 1){
    for(i in 1:partition){
      initial = ((i-1)*div)
      end = initial + div
      if(i == 1){
        initial = 1
        end = div
      }
      aux = serie[initial:end]
      probability = distribution(aux,dimension,delay)
      entropy[i] = shannonEntropyNormalized(probability)
      complexity[i] = Ccomplexity(probability)
    }
  }
  else{
    probability = distribution(serie,dimension,delay)
    entropy = shannonEntropyNormalized(probability)
    complexity = Ccomplexity(probability)
  }
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  #png("myHC.png")
  p = qplot(x=c2x,y=c2y,geom="line",xlab="Shannon Entropy",ylab="MPR Statistical Complexity") +
    ggtitle("Entropy-Complexity Plane") + theme(plot.title = element_text(hjust=0.5)) +
    geom_line(aes(x=c1x,c1y)) + geom_point(aes(x=entropy,y=complexity),color="blue")
  print(p)
  #dev.off()
  data = matrix(nrow = partition, ncol = 2)
  data[,1] = entropy
  data[,2] = complexity
  return(data)
}
