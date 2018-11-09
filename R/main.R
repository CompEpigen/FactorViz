# base file to call from commad line
setwd("/home/reaper/epigen/FactorViz/FactorViz2.0")
library(MeDeCom)
source("app.R", local=T)
startFactorViz<-function(object, input.data=NULL) { 
  runApp(app)
}
startFactorViz(NULL)  
