#' startFactorViz
#' This function intialises factorviz and start
#'
#' @param path takes in the directory path to decomp output
#' @export
#'
startFactorViz <- function(path=NULL) {
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
      onstartLoad(path=path)
    }
  )
  shiny::runApp(app, port=6578)
}
