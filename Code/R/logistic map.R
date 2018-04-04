logisticMap<-function(n,x0,r,precision){
  map = rep(0,n)
  options(digits=precision)
  map[1] = x0
  map[1] = round(map[1],precision+1)
  for(i in 1:(n-1)){
    map[i+1] = r*map[i]*(1-map[i])
    map[i+1] = round(map[i+1],precission+1)
  }
  map
}
