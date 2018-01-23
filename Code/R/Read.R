#Leitura de dados em arquivos#

#Leitura do arquivo .txt e armazenamento do conteúdo em um array#
Read_txt<-function(name,column){
  data = read.table(name, stringsAsFactors=FALSE, fileEncoding="latin1")
  data = data[,column]
  if(mode(data)=="character"){
    data = type.convert(data)
  }
  data = na.omit(data)
  return(data)
}

Read_txt2<-function(column){
  data = read.table(file.choose())
  data = data[,column]
  data = na.omit(data)
  return(data)
}

#Leitura do arquivo .csv e armazenamento do conteúdo em um array#
Read_csv<-function(column,separador=";"){
  data=read.csv(file.choose(), stringsAsFactors=T, fileEncoding="latin1",sep=separador)
  data = data[,column]
  if(mode(data)=="character"){
    data = type.convert(data)
  }
  data = na.omit(data)
  return(data)
}

#option == 0 -> Manter os valores repetidos
#Option != 0 -> Retirar os valores repetidos
Read_csv_interface<-function(file, column, option = 0){  
    time=read.csv(svalue(file), stringsAsFactors=T, fileEncoding="latin1",sep=";")
    time = time[,column]
    if(mode(time)=="character"){
      time = type.convert(time)
    }
    time = na.omit(time)
    if(option!=0) time = unq
    return(time)
}
