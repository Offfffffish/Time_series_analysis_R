library(ggplot2)
library(combinat)

partition<-function(series,size,delay){
  myPartition = matrix(ncol=size,nrow=ceiling(length(series)/delay))
  col = counter = i = 1
  init =0 
  if(length(levels(series))!=0){
    series = factor(series)
  }
  while(i <= length(series)){
    myPartition[counter,col] = series[i]
    col = col + 1
    if((col - 1) == size){
      i = init = init + delay 
      col = 1
      counter = counter + 1
      if((i + size + 1) > length(series)){
        counter = counter - 1
        break
      }
    }
    i = i + 1
  }
  myPartition[1:(counter),]
}
entropyPlane<-function(serie,partition,dimension,delay,option1,option2,q=0){
  entropy = rep(0,partition)
  division = length(serie)/partition
  division = floor(division)
  cont = 1
  rest = 0
  if(partition*division < length(serie)){
    rest = length(serie) - (partition*division)
  }
  init = 1
  final = 0
  for(i in 1:partition){
    final = final + division
    if(cont <= rest){
      final = final +  1
    }
    myPartition = serie[init:final]
    if(option1 == 1){
      probability = distribution(myPartition,dimension,delay)
    }else{
      probability = WPE(myPartition,dimension,delay)
    }
    if(option2 == 1){
      entropy[i] = shannonEntropyNormalized(probability)
    }else if(option2 == 2){
      entropy[i] = tsallisEntropy(probability,q)
    }else if(option2 == 3){
      entropy[i] = renyiEntropy(probability,q)
    }else{
      entropy[i] = PMEUnidimensional(probability)
    }
    init = final + 1
    cont = cont + 1
  }
  png("myEntropy.png")
  if(partition==1){
    p = qplot(x=c(1:partition),y=entropy,geom="point",xlab="Partitions-Time Series",ylab="Entropy") +
      ggtitle("Permutation Entropy Evolution") + theme(plot.title = element_text(hjust=0.5))
  }
  else{
    p = qplot(x=c(1:partition),y=entropy,geom="line",xlab="Partitions-Time Series",ylab="Entropy") +
      ggtitle("Permutation Entropy Evolution") + theme(plot.title = element_text(hjust=0.5))
  }
  print(p)
  dev.off()
  return(entropy)
}

distancePlane<-function(partitions,dimension,delay,option=1,optionP=1,q=1){
  numberPartitions = dim(partitions)[1]
  if(is.null(numberPartitions)){
    numberPartitions = 1
  }
  distance = rep(0,numberPartitions)
  probability = matrix(nrow=numberPartitions,ncol=factorial(dimension))
  if(optionP == 1){
    if(numberPartitions==1){
      probability[1,] = distribution(partitions,dimension,delay)
    }
    else{
      for(i in 1:numberPartitions){
        probability[i,] = distribution(partitions[i,],dimension,delay)
      }
    }
  }else if(optionP == 2){
    if(numberPartitions==1){
      probability[1,] = WPE(partitions,dimension,delay)
    }
    else{
      for(i in 1:numberPartitions){
        probability[i,] = WPE(partitions[i,],dimension,delay)
      }
    }
  }else{
    cat("Distance option unavailable\n")
  }
  if(option == 1){
    for(i in 1:numberPartitions){
      distance[i] = euclidian_distance(probability[i,])
    }
  }else if(option == 2){
    for(i in 1:numberPartitions){
      distance[i] = euclidian_quadratica_distance(probability[i,])
    }
  }else if(option == 3){
    for(i in 1:numberPartitions){
      distance[i] = manhattan_distance(probability[i,])
    }
  }else if(option == 4){
    for(i in 1:numberPartitions){
      distance[i] = chebyshev_distance(probability[i,])
    }
  }else if(option == 5){
    for(i in 1:numberPartitions){
      distance[i] = kullback_leibler_divergence(probability[i,])
    }
  }else if(option == 6){
    for(i in 1:numberPartitions){
      distance[i] = hellinger_Distance(probability[i,])
    }
  }else if(option == 7){
    for(i in 1:numberPartitions){
      distance[i] = jensenDivergence(probability[i,])
    }
  }else if(option == 8){
    for(i in 1:numberPartitions){
      distance[i] = wootters_distance(probability[i,],q)
    }
  }else{
    cat("Distance option unavailable\n")
  }
  print(distance)
  if(numberPartitions==1){
    qplot(x=c(1:numberPartitions),y=distance,geom="point",xlab="Partitions-Time Series",ylab="Distance") +
      ggtitle("Stochastic distance Evolution") + theme(plot.title = element_text(hjust=0.5))
  }
  else{
    qplot(x=c(1:numberPartitions),y=distance,geom="line",xlab="Partitions-Time Series",ylab="Distance") +
      ggtitle("Stochastic distance Evolution") + theme(plot.title = element_text(hjust=0.5))
  }
}








