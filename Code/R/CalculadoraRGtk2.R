require(RGtk2)

window <- gtkWindow()
window["title"] <- "Calculator"

frame <- gtkFrameNew("Calculate")
window$add(frame)

box1 <- gtkVBoxNew()
box1$setBorderWidth(30)
frame$add(box1)   #add box1 to the frame

box2 <- gtkHBoxNew(spacing= 10) #distance between elements
box2$setBorderWidth(24)

TextToCalculate<- gtkEntryNew() #text field with expresion to calculate
TextToCalculate$setWidthChars(25)
box1$packStart(TextToCalculate)

label = gtkLabelNewWithMnemonic("Result") #text label
box1$packStart(label)

result<- gtkEntryNew() #text field with result of our calculation
result$setWidthChars(25)
box1$packStart(result)

box2 <- gtkHBoxNew(spacing= 10) # distance between elements
box2$setBorderWidth(24)
box1$packStart(box2)

Calculate <- gtkButton("Calculate")
box2$packStart(Calculate,fill=F) #button which will start calculating

Sin <- gtkButton("Sin") #button to paste sin() to TextToCalculate
box2$packStart(Sin,fill=F)

Cos <- gtkButton("Cos") #button to paste cos() to TextToCalculate
box2$packStart(Cos,fill=F)

model<-rGtkDataFrame(c("double","integer"))
combobox <- gtkComboBox(model)
#combobox allowing to decide whether we want result as integer or double

crt <- gtkCellRendererText()
combobox$packStart(crt)
combobox$addAttribute(crt, "text", 0)

gtkComboBoxSetActive(combobox,0)
box2$packStart(combobox)
DoCalculation<-function(button)
{
  
  if ((TextToCalculate$getText())=="") return(invisible(NULL)) #if no text do nothing
  
  #display error if R fails at calculating
  tryCatch(
    if (gtkComboBoxGetActive(combobox)==0)
      result$setText((eval(parse(text=TextToCalculate$getText()))))
    else (result$setText(as.integer(eval(parse(text=TextToCalculate$getText()))))),
    error=function(e)
    {
      ErrorBox <- gtkDialogNewWithButtons("Error",window, "modal","gtk-ok", GtkResponseType["ok"])
      box1 <- gtkVBoxNew()
      box1$setBorderWidth(24)
      ErrorBox$getContentArea()$packStart(box1)
      
      box2 <- gtkHBoxNew()
      box1$packStart(box2)
      
      ErrorLabel <- gtkLabelNewWithMnemonic("There is something wrong with your text!")
      box2$packStart(ErrorLabel)
      response <- ErrorBox$run()
      
      
      if (response == GtkResponseType["ok"])
        ErrorBox$destroy()
      
    }
  )
  
}


PasteSin<-function(button)
{
  TextToCalculate$setText(paste(TextToCalculate$getText(),"sin()",sep=""))
  
}

PasteCos<-function(button)
{
  TextToCalculate$setText(paste(TextToCalculate$getText(),"cos()",sep=""))
  
}

#however button variable was never used inside 
#functions, without it gSignalConnect would not work
gSignalConnect(Calculate, "clicked", DoCalculation)
gSignalConnect(Sin, "clicked", PasteSin)
gSignalConnect(Cos, "clicked", PasteCos)