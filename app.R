#library(shiny)
library(shinyFiles)
library(shinyjs)
#library(R.utils)
source('js_css.R', local=T)
source('ui.R', local = T)
source('base_server.R', local = T)
source('global.R')
app<-shinyApp(
  ui = baseUI,
  server = base_server
  #options(shiny.trace=F, shiny.autoreload=TRUE)
)