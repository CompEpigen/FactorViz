# base file to call from commad line

# startFactorViz
# @export
startFactorViz<-function(object, input.data=NULL) {
	app<-shiny::shinyApp(
  ui = baseUI,
  server = base_server)
  shiny::runApp(app)
}
