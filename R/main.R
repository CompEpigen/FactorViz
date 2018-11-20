#' startFactorViz
#' This function intialises factorviz and start, it accepts no parameters
#' @export
#'
startFactorViz <- function() {
  require(shiny)
  require(shinyFiles)
  require(shinythemes)
  require(ggplot2)
  require(gplots)
  require(RColorBrewer)
  require(grid)
  require(gridExtra)
  require(DT)
  require(RnBeads)
  require(MeDeCom)
  app <- shiny::shinyApp(
    ui = baseUI,
    server = base_server,
    onStart <-function(){
      onstartLoad()
    }
    port=6578,
  )
  shiny::runApp(app)
}
