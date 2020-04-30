#' startFactorViz
#' This function intialises factorviz and start
#'
#' @param decomp_output takes in the directory path to decomp output
#' @param medecom_set takes in the path to medecom set file or the MeDeCom Set object
#' @param ann_C takes in the path to CpG Annotation file or the data.frame of CpG Annotations
#' @param ann_S takes in the path to Sample Annotation file or the data.frame of Sample Annotations
#' @param ref_meth takes in the path to Reference Methylome file or the data.frame of Reference Methylome
#' @param port The port to be used for starting the interface in a webbrowser. If multiple parallel sessions are to be started, be sure to specify different ports.
#' @details
#' If decomp_output provided all other parameters force set to NULL
#'
#' @export

startFactorViz <- function(decomp_output=NULL, medecom_set=NULL, ann_C=NULL, ann_S=NULL, ref_meth=NULL, port=6578) {
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
  shiny::runApp(app, port=port)
}
