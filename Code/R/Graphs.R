library(combinat)
library(ggplot2)
library(dygraphs)

time_series<-function(serie,cont){ 
  if(cont%%2==0){
    png("myplot.png")
  }else{
    png("myplot2.png")
  }
  p = qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Time",ylab="Serie") +
    ggtitle("Graphic of time serie") + theme(plot.title = element_text(hjust=0.5))
  print(p)
  dev.off()
}

#Points == 0 -> first point
#Points != 0 -> all points
patternsOnGraph<-function(serie,dimension,delay,number_of_pattern,points = 0){
  lengthW=0
  point_time=point_value=c(1:length(serie))
  p_patterns <- formationPattern(serie,dimension,delay)
  n_symbols <- dim(p_patterns)[1]
  symbols <- definePatterns(dimension)
  elements <- formationPattern(serie,dimension,delay,2)
  index <- formationPattern(serie,dimension,delay,3)
  for(i in 1:n_symbols){
    if(all(p_patterns[i,]==symbols[number_of_pattern,])){
      if(points==0){
        lengthW = lengthW + 1
        point_value[lengthW]=elements[i,1]
        point_time[lengthW]=index[i,1]
      }
      else{
        for(j in 1:dimension){
          lengthW=lengthW+1
          point_value[lengthW]=elements[i,j]
          point_time[lengthW]=index[i,j]
        }
      }
    }
  }
  png("myPattern.png")
  if(lengthW!=0){
    p =qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Time",ylab="Serie") +
      ggtitle("Graphic of time serie") + theme(plot.title = element_text(hjust=0.5)) +
      geom_point(aes(x=point_time[1:lengthW],y=point_value[1:lengthW]),color="blue")
  }else{
    p =qplot(x=c(1:length(serie)),y=serie,geom="line",xlab="Time",ylab="Serie") +
      ggtitle("Graphic of time serie") + theme(plot.title = element_text(hjust=0.5))
  }
  print(p)
  dev.off()
}

histogram<-function(serie,dimension,delay){
  fat=factorial(dimension)
  p_patterns <- formationPattern(serie,dimension,delay)
  n_symbols <- dim(p_patterns)[1]
  symbol <- definePatterns(dimension)
  index_rep=array(0,n_symbols)
  for(i in 1:n_symbols){
    for(j in 1:fat){
      if(all(p_patterns[i,]==symbol[j, ])){
        index_rep[i]=j
        break
      }
    }
  }
  index_rep=index_rep[1:n_symbols]
  png("myHistogram.png")
  p = qplot(index_rep,geom="histogram",xlab="Patterns",ylab="Probability",binwidth=1) +
    ggtitle("Histogram of the patterns") + theme(plot.title = element_text(hjust=0.5))
  print(p)
  dev.off()
  return(symbol)
}

histogramImage<-function(myImg,dimx,dimy,delx,dely,dimension=0){  
  if(!dimension){
    dimension=dim(myImg)[1]
  }
  d = dimX*dimY
  fat = factorial(d)
  p_patterns <- formationPatternsImage(myImg,dimX,dimY,delX,delY,dimension)
  n_symbols <- formationPatternsImage(myImg,dimX,dimY,delX,delY,dimension, 1)
  symbols <- definePatterns(d)
  index_rep=array(0,n_symbols)
  for(i in 1:n_symbols){
    for(j in 1:fat){
      if(all(p_patterns[i,]==symbols[j, ])){
        index_rep[i]=j
        break
      }
    }
  }
  index_rep=index_rep[1:n_symbols]
  p = qplot(index_rep,geom="histogram",xlab="Patterns",ylab="Probability",binwidth=1) +
    ggtitle("Histogram of the patterns") + theme(plot.title = element_text(hjust=0.5))
  print(p)
  print(symbols)
}

