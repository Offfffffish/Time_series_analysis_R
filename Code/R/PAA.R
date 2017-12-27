library(ggplot2)

paa <- function(series,size){
  res = rep(0,size)
  for(i in 0:(length(series)*size-1)){
    res[i%/%length(series)+1] = res[i%/%length(series)+1] + series[i%/%size+1]
  }
  for(i in 1:size){
    res[i]=res[i]/length(series)
  }
  res
}

plotPAA<-function(series,size,option=0){
  vline=seq(from=0,to=length(series),by=(length(series)/size))
  segm = rep(0,size+1)
  segm[1:size] = paa(series,size)
  segm[size+1] = segm[size]
  steps = data.frame(x=vline,y=segm)
  segm = as.double(format(round(segm,2),nsmall=2))
  myText = segm[1:size]
  png("myPAA.png")
  p = qplot(x=c(1:length(series)),y=series,geom="line",xlab="Time",ylab="Serie",colour="red") +
      ggtitle("Piecewise Aggregate Approximation") + theme(plot.title = element_text(hjust=0.5)) +
      geom_step(data=steps,aes(x=x,y=y),colour="black") +
      geom_text(aes(x=(vline[1:(length(vline)-1)]+vline[2:length(vline)])/2,y=segm[1:(length(segm)-1)]*1.05,label=myText,colour="blue"))
  print(p)
  dev.off()
  return(segm[1:size])
}

sum_of_variation<-function(segment){
  c1 = segment[1:(length(segment)-1)]
  c2 = segment[2:length(segment)]
  res = sum(abs(c1-c2))
  res
}

ssv<-function(series,size){
  aux = matrix(nrow=round(length(series)/size),ncol=size)
  res = rep(0,round(length(series)/size))
  ini = row = sum = i = 1
  while((i <= length(series)) && (row <= round(length(series)/size))){
    aux[row,sum] = series[i]
    if(sum == size){
      row = row + 1
      sum = 1
      i = ini + size - 1
      ini = i
    }else{
      i = i + 1
      sum = sum + 1
    }
  }
  for(i in 1:round(length(series)/size)){
    res[i] = sum_of_variation(aux[i,])
  }
  res
}

