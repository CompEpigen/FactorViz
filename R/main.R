# base file to call from commad line

# starttest
# @export

#source("app.R", local=T)
startFactorViz <- function() {
  app <- shiny::shinyApp(
    ui = baseUI,
    server = base_server
  )
  
  shiny::runApp(app)
}
#startFactorViz()
