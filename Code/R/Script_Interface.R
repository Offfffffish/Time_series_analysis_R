source(file.path('Graphs.R'))
source(file.path('Functions.R'))
source(file.path('Entropys.R'))
source(file.path('complexity-entropy.R'))
source(file.path('Distances.R'))
source(file.path('Equalities-PME.R'))
source(file.path('newPatterns.R'))
source(file.path('PAA.R'))
source(file.path('Partition&EntropyPlane.R'))
source(file.path('PBWeigth.R'))
source(file.path('PIP.R'))
source(file.path('Read.R'))
source(file.path('SAX.R'))
library(ggplot2)
library(dygraphs)
require(gWidgets2)
require(gWidgetsRGtk2)
require(RGtk2)
options(guiToolkit="RGtk2")

contFiles = 0
write.table(contFiles,file="AuxResult.txt")
win=gwindow("TIME SERIES ANALYSIS  ")
img <- gdkPixbufNewFromFile("icone.png")
getToolkitWidget(win)$setIcon(img$retval)
paned<-gpanedgroup(cont=win)
group<- ggroup (cont = paned , horizontal = FALSE )
#getBlock(win)$modifyBg(GtkStateType["normal"], "blue")

#---------------------------------------XXX-------------------------------------------
frame1 <- gframe ( "PLOT:" , cont = paned , horizontal = TRUE)
tbl1=glayout(cont=frame1)
tbl1[2,2]=img <- gimage("grafico.png")
size(frame1)<-c(500,530)
#------------------------------------frame to enter the plot-----------------------------
frame <- gframe ( "RESULT:" , cont = group, horizontal = TRUE ,expand=TRUE)
size(frame)<-c(480,150)
tbl=glayout(cont=frame)
tbl[2,2] <- glabel("VALUE OF THE RESULT")
tbl[2,14] <- (text1 <- gedit("",container=tbl,coerce.with=as.numeric))
tbl[4,2] <- glabel("TIME SERIES SIZE")
tbl[4,14] <- (text2 <- gedit("",container=tbl,coerce.with=as.numeric))
tbl[6,2] <- glabel("PERCENTAGE OF EQUAL VALUES")
tbl[6,14] <- (text3 <- gedit("",container=tbl,coerce.with=as.numeric))
#---------------------------------------XXX-------------------------------------------
frame2<- gframe ("PARAMETERS:",cont=group,horizontal=TRUE,expand=TRUE)
size(frame2)<-c(480,500)
tbl2=glayout(cont=frame2)
dist = c("Band and Pompe" , "Bandt and Pompe weigth")
dim = c("3","4","5","6")
fun = c("Shannon Entropy" , "Tsallis Entropy", "Renyi Entropy","Euclidian Distance","Quadratic Euclidian Distance"
    ,"Manhattan Distance","Chebyshev Distance","Hellinger Distance","Jensen Divergence","Wootter Distance","Kullback Leibler Divergence",
    "Statistical Complexity","Permutation Min Entropy","Symbolic Aggregate Approximation","Perceptually Important Points","Piecewise Aggregate Approximation",
    "Bandt and Pompe Weigth","Entropy Plane","HC Plane","Time Series Plane","Histogram")
tbl2[2,2] = myFile = gfilebrowse (text = "Select file...", type = "open", quote = FALSE,
                         filter = list("Text File" = list(patterns = c("*.csv"))),      
                         container = tbl)
exportOptions = c("YES", "NO")
tbl2[4,2] <- glabel("EXPORT RESULT")
tbl2[4,3] <- (cb0 <- gcombobox(exportOptions,container=tbl2))
tbl2[6,2] <- glabel("FUNCTION")
tbl2[6,3] <- (cb1 <- gcombobox(fun,container=tbl2))
tbl2[8,2] =glabel("DIMENSION")
tbl2[8,3] <- (cb2 <- gcombobox(dim, cont=tbl2))
tbl2[10,2] =glabel("DELAY")
tbl2[10,3]<- (text2 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[12,2] =glabel("Q")
tbl2[12,3] <- (text3 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[14,2] =glabel("PATTERN")
tbl2[14,3] <- (text4 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[16,2] =glabel("LETTERS")
tbl2[16,3] <- (text5 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[18,2] =glabel("PARTITIONS")
tbl2[18,3] <- (text6 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[20,2] =glabel("NUMBER OF POINTS")
tbl2[20,3] <- (text7 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[22,2] =glabel("DISTRIBUTION")
tbl2[22,3] <- (cb3 <- gcombobox(dist, cont=tbl2))
#---------------------------------my function -----------------------------------------

tbl2[2,3] <- gbutton("CALCULATE",container=tbl,handler=function(a=1,b=2){
  name = "grafico.png"
  missingParameter = "The desired functionality could not be performed, the parameters are missing:"
  myResult = svalue(cb1,index=TRUE)
  if(svalue(myFile) != "Select file..."){
    time=read.csv(svalue(myFile), stringsAsFactors=T, fileEncoding="latin1",sep=";")
    time = time[,2]
    if(mode(time)=="character"){
      time = type.convert(time)
    }
    time = na.omit(time)
    size = length(time)
    equals = equalitiesValues(time) 
    alert = finalResult = 0
    if((myResult == 4) ||(myResult == 5) ||(myResult == 6) || (myResult == 7) ||(myResult == 8) ||(myResult == 12) ||
       (myResult == 1) ||(myResult == 13) ||(myResult == 21) ||(myResult == 9)){ 
      if(!is.na(svalue(text2))){
        if(svalue(cb3,index=TRUE)==1){
          probability = distribution(time,svalue(cb2,index=TRUE)+3,svalue(text2),1)
        }else{
          probability = WPE(time,svalue(cb2,index=TRUE)+3,svalue(text2))
        }
        if(myResult == 4){
          finalResult = euclidian_distance(probability)
        }else if(myResult == 5){
          finalResult = euclidian_quadratica_distance(probability)
        }else if(myResult == 6){
          finalResult = manhattan_distance(probability)
        }else if(myResult == 7){
          finalResult = chebyshev_distance(probability)
        }else if(myResult == 8){
          finalResult = hellinger_Distance(probability)
        }else if(myResult == 12){
          finalResult = Ccomplexity(probability)
        }else if(myResult == 1){
          finalResult = shannonEntropyNormalized(probability)
        }else if(myResult == 13){
          finalResult = PMEUnidimensional(probability)
        }else if(myResult == 21){
          histogram(time,svalue(cb2,index=TRUE)+3,svalue(text2))
          name = "myHistogram.png"
        }else if(myResult == 9){
          finalResult = jensenDivergence(probability)
        }
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Dimension, delay and distribution.")
      }
    }else if((myResult == 11) ||(myResult == 10) ||(myResult == 2) || (myResult == 3)){ 
      if((!is.na(svalue(text2)))&&(!is.na(svalue(text3)))){
        if(svalue(cb3,index=TRUE)==1){
          probability = distribution(time,svalue(cb2,index=TRUE)+3,svalue(text2),1)
        }else{
          probability = WPE(time,svalue(cb2,index=TRUE)+3,svalue(text2))
        }
        if(myResult == 11){
          finalResult = kullback_leibler_divergence(probability)
        }else if(myResult == 10){
          finalResult = wootters_distance(probability,svalue(text3))
        }else if(myResult == 2){
          finalResult = tsallisEntropy(probability,svalue(text3))
        }else if(myResult == 3){
          finalResult = renyiEntropy(probability,svalue(text3))
        }
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Dimension, delay, q and distribution.")
      }
    }
    #else if((myResult == 21)){
     # if((!is.na(svalue(text4)))&&(!is.na(svalue(text2)))&&(!is.na(svalue(cb2,index=TRUE)))){
      #  patternsOnGraph(time,svalue(cb2,index=TRUE)+3,svalue(text2),svalue(text4))
       # name = "myPattern.png"
      #}else{
       # alert = 1
       # missingParameter = paste(missingParameter," Dimension, delay and Pattern")
      #}
    #}
    else if((myResult == 14)){
      if((!is.na(svalue(text5)))&&(!is.na(svalue(text6)))){
        saxPlot(time,svalue(text5),svalue(text6))
        name = "mySAX.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Letters and partitions.")
      }
    }else if((myResult == 15)){
      if((!is.na(svalue(text7)))){
        finalResult  = PIP(time,svalue(text7))
        name = "myPIP.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Number of points.")
      }
    }else if((myResult == 16)){
      if((!is.na(svalue(text6)))){
        finalResult  = plotPAA(time,svalue(text6))
        name = "myPAA.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Partitions.")
      }
    }else if((myResult == 18)){
      if((!is.na(svalue(text2)))&&(!is.na(svalue(text6)))&&(!is.na(svalue(cb2,index=TRUE)))){
        finalResult  = entropyPlane(time,svalue(text6),svalue(cb2,index=TRUE)+3,svalue(text2),1,1,0)
        name = "myEntropy.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Dimension, delay and partitions.")
      }
    }else if((myResult == 19)){    
      if((!is.na(svalue(text2)))&&(!is.na(svalue(text6)))){
        dataP  = partitionMPR(time,svalue(cb2,index=TRUE)+3,svalue(text2),svalue(text6))
        finalResult = "\n Entropy \t Complexity \n"
        for(i in 1:dim(dataP)[1]){
          dataPA = paste(toString(dataP[i,1:2]),"\n")
          finalResult = paste(finalResult,dataPA)
        }
        name = "myHC.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Dimension, delay and partitions.")
      }
    }else if(myResult == 20){
      time_series(time)
      name = "myplot.png"
      
    }
  }else{
        alert = 1
        missingParameter = paste(missingParameter," Time series file.")    
  }
  if(alert==1){
    dialog <- gtkMessageDialog(NULL, "destroy-with-parent","info", "ok", missingParameter)
    if (dialog$run() == GtkResponseType["ok"]) dialog$destroy()
  }
  #result = as.character(finalResult)
  result = toString(finalResult)
  svalue(img)<- name 
  tbl[6,14] <- (text3 <- gedit(equals,container=tbl,coerce.with=as.numeric))
  if(myResult != 21 && myResult != 18 && myResult!=15 && myResult!=16 && myResult!=19)   tbl[2,14] <- (text1 <- gedit(result,container=tbl,coerce.with=as.numeric))
  tbl[4,14] <- (text2 <- gedit(size,container=tbl,coerce.with=as.numeric))
  if(svalue(cb0,index=TRUE) == 1){
    nameResult = "result"
    contFiles = read.table("AuxResult.txt")
    if(contFiles != 0){
      nameResult = paste(nameResult,toString(contFiles))
    } 
    nameResult = paste(nameResult,".txt")
    contFiles = contFiles + 1
    write.table(contFiles,"AuxResult.txt")
    cat(paste("TIME SERIES SIZE:",size), "\n", file=nameResult, append=FALSE, sep='')
    cat(paste("PERCENTAGE OF EQUALS VALUES:",equals), "\n", file=nameResult, append=TRUE, sep='')
    cat(paste("FUNCTION:",fun[myResult]), "\n", file=nameResult, append=TRUE, sep='')
    cat(paste("RESULT:",result), "\n", file=nameResult, append=TRUE, sep='')
  }
})

