#' startFactorViz
#' This function intialises factorviz and start
#'
#' @param decomp_output takes in the directory path to decomp output
#' @param medecom_set takes in the path to medecom set file
#' @param ann_C takes in the path to CpG Annotation file
#' @param ann_S takes in the path to Sample Annotation file
#' @param ref_meth takes in the path to Reference Methylome file
#' @details
#' If decomp_output provided all other parameters force set to NULL
#'
#' @export

startFactorViz <- function(decomp_output=NULL, medecom_set=NULL, ann_C=NULL, ann_S=NULL, ref_meth=NULL) {
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
      onstartLoad(decomp_output=decomp_output, medecom_set=medecom_set, ann_C=ann_C, ann_S=ann_S, ref_meth=ref_meth )
    }
  )
  shiny::runApp(app, port=6578)
}
