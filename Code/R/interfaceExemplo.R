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

cont = 0
write.table(cont,file="GraphControl.txt")
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
size(frame)<-c(480,180)
tbl=glayout(cont=frame)
fun = c("Shannon Entropy" , "Tsallis Entropy", "Renyi Entropy","Euclidian Distance","Quadratic Euclidian Distance"
    ,"Manhattan Distance","Chebyshev Distance","Hellinger Distance","Jensen Divergence","Wootter Distance","Kullback Leibler Divergence",
    "Statistical Complexity","Permutation Min Entropy","Symbolic Aggregate Approximation","Perceptually Important Points","Piecewise Aggregate Approximation",
    "Bandt and Pompe Weigth","Equalities Values","Entropy Plane","HC Plane","Time Series Plane","Patterns on Graph","Histogram")
tbl[2,2] = myFile = gfilebrowse (text = "Select file...", type = "open", quote = FALSE,
                         filter = list("Text File" = list(patterns = c("*.csv"))),      
                         container = tbl)
tbl[4,2] <- glabel("FUNCTION")
tbl[4,3] <- (cb1 <- gcombobox(fun,container=tbl))
tbl[6,2] <- glabel("RESULT")
tbl[6,3] <- (text1 <- gedit("",container=tbl,coerce.with=as.numeric))
#tbl[6,3] <- (text1 <- gtext("",container = tbl,width = 100,height = 50))
#---------------------------------------XXX-------------------------------------------
frame2<- gframe ("PARAMETERS:",cont=group,horizontal=TRUE,expand=TRUE)
size(frame2)<-c(480,380)
tbl2=glayout(cont=frame2)
dist = c("Band and Pompe" , "Bandt and Pompe weigth")
dim = c("3","4","5","6")
tbl2[2,2] =glabel("DIMENSION")
tbl2[2,7] <- (cb2 <- gcombobox(dim, cont=tbl2))
tbl2[4,2] =glabel("DELAY")
tbl2[4,7]<- (text2 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[6,2] =glabel("Q")
tbl2[6,7] <- (text3 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[8,2] =glabel("PATTERN")
tbl2[8,7] <- (text4 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[10,2] =glabel("LETTERS")
tbl2[10,7] <- (text5 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[12,2] =glabel("PARTITIONS")
tbl2[12,7] <- (text6 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[14,2] =glabel("NUMBER OF POINTS")
tbl2[14,7] <- (text7 <- gedit("", container=tbl2,coerce.with=as.numeric))
tbl2[16,2] =glabel("DISTRIBUTION")
tbl2[16,7] <- (cb3 <- gcombobox(dist, cont=tbl2))
#---------------------------------my function -----------------------------------------
tbl[2,3] <- gbutton("CALCULATE",container=tbl,handler=function(a=1,b=2){
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
    alert = finalResult = 0
    if((myResult == 4) ||(myResult == 5) ||(myResult == 6) || (myResult == 7) ||(myResult == 8) ||(myResult == 12) ||
       (myResult == 1) ||(myResult == 13) ||(myResult == 23) ||(myResult == 9)){ 
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
        }else if(myResult == 17){
          finalResult = WPE(time,svalue(cb2,index=TRUE)+3,svalue(text2))
        }else if(myResult == 23){
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
    }else if((myResult == 22)){
      if((!is.na(svalue(text4)))&&(!is.na(svalue(text2)))&&(!is.na(svalue(cb2,index=TRUE)))){
        patternsOnGraph(time,svalue(cb2,index=TRUE)+3,svalue(text2),svalue(text4))
        name = "myPattern.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Dimension, delay and Pattern")
      }
    }else if((myResult == 14)){
      if((!is.na(svalue(text5)))&&(!is.na(svalue(text6)))){
        saxPlot(time,svalue(text5),svalue(text6))
        name = "mySAX.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Letters and partitions.")
      }
    }else if((myResult == 15)){
      if((!is.na(svalue(text7)))){
        PIP(time,svalue(text7))
        name = "myPIP.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Number of points.")
      }
    }else if((myResult == 16)){
      if((!is.na(svalue(text6)))){
        plotPAA(time,svalue(text6))
        name = "myPAA.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Partitions.")
      }
    }else if((myResult == 19)){
      if((!is.na(svalue(text2)))&&(!is.na(svalue(text6)))&&(!is.na(svalue(cb2,index=TRUE)))){
        #dialog <- gtkMessageDialog(NULL, "destroy-with-parent","question", "ok", "What entropy and distribution should be used in the process?")
        #choicesE <- c("Shannon entropy" , "Tsallis entropy", "Renyi entropy","Min entropy")
        #radio_buttons <- NULL
        #vbox <- gtkVBox(TRUE, 2)
        #for (choice in choicesE) {
         # button <- gtkRadioButton(radio_buttons, choice)
         # vbox$add(button)
         # radio_buttons <- c(radio_buttons, button)
        #}
        #frame <- gtkFrame("Entropys")
        #frame$add(vbox)
        #dialog[["vbox"]]$add(frame)
        #if (dialog$run() == GtkResponseType["ok"]) dialog$destroy()
        entropyPlane(time,svalue(text6),svalue(cb2,index=TRUE)+3,svalue(text2),1,1,0)
        name = "myEntropy.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Dimension, delay and partitions.")
      }
    }else if((myResult == 20)){    
      if((!is.na(svalue(text2)))&&(!is.na(svalue(text6)))){
        partitionMPR(time,svalue(cb2,index=TRUE)+3,svalue(text2),svalue(text6))
        name = "myHC.png"
      }else{
        alert = 1
        missingParameter = paste(missingParameter," Dimension, delay and partitions.")
      }
    }else if(myResult == 18){ 
      finalResult = equalitiesValues(time)
    }else if(myResult == 21){
      cont = read.table("GraphControl.txt")
      time_series(time,cont)
      if(cont%%2==0){
        name = "myplot.png"
      }else{
        name = "myplot2.png"
      }
      cont=cont+1
      write.table(cont,file="GraphControl.txt")
      
    }
  }else{
        alert = 1
        missingParameter = paste(missingParameter," Time series file.")    
  }
  if(alert==1){
    dialog <- gtkMessageDialog(NULL, "destroy-with-parent","info", "ok", missingParameter)
    if (dialog$run() == GtkResponseType["ok"]) dialog$destroy()
  }
  result = as.character(finalResult)
  svalue(img)<- name
  tbl[6,3] <- (text1 <- gedit(result,container=tbl,coerce.with=as.numeric))
  #tbl[6,3] <- (text1 <- gtext(result,container = tbl,width = 100,height = 50))
})

