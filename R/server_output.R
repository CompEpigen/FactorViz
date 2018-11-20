#This file contains the output objects of the server

#############################################
########### main panel outputs ##############
#############################################

##########################################  Home tab main panel
server_output <- function(input, output, server_env) {
  output$files <- renderPrint(
      list.files(server_env$path())
    )

  output$AnalysisRunDescriptionHeader <- renderUI({
    server_env$df()
    if (MEDSET_FLAG) {
      wellPanel(#strong("Analysis Run:"),
        h4(server_env$getAnalysisName()))
    } else{
      wellPanel(strong("Please load a dataset"))
    }
  })

  output$AnalysisRunParameterTable <- renderTable({
    server_env$df()
    if (MEDSET_FLAG) {
      withProgress(
        message = 'Loading datasets in progress\n',
        detail = 'This may take a while...',
        value = 0,
        style = getShinyOption('old'),
        {
          output <- list()
          print("Starting table")
          results <- server_env$dataset()
          print("...............................MeDeComSet...........................")
          print(
            "...............................MeDeComSet_ende..........................."
          )

          for (name in names(ANALYSIS_META_INFO[["analysis_params"]])) {
            incProgress(1 / length(names(ANALYSIS_META_INFO[["analysis_params"]])))
            display_name <-
              ANALYSIS_META_INFO[["analysis_params"]][name]
            if (name %in% names(results@parameters)) {
              val <- results@parameters[[name]]
              if (name %in% c("lambdas")) {
                val <- sort(val)
              }

              if (is.integer(val)) {
                val <- as.character(val)
              } else{
                if (name %in% names(ANALYSIS_META_INFO[["param_extensions"]])) {
                  val_ext <- ANALYSIS_META_INFO[["param_extensions"]][[name]][val]
                  if (!is.na(val_ext)) {
                    val <- val_ext
                  }
                }
              }
            } else if (name %in% "NO_OF_SAMPLES") {
              val <- as.character(ncol(server_env$getMethData()))
            } else if (name %in% "REFERENCE_PROFILES") {
              val <- paste(colnames(server_env$getTrueT()), collapse = ", ")
            } else{
              val = ""
            }
            if (length(val) == 0) {
              val = ""
            }
            if (is.recursive(val) || val != "") {
              output[[name]] <- c(display_name, paste(val, collapse = ", "))
            }
          }
          table <- do.call("rbind", output)
          rownames(table) <- NULL
          colnames(table) <- c("Parameter", "Value")
          table
        }
      )
    } else{

    }
  }, width = 1000, include.rownames = FALSE)

  ##################################### K selection main panel
  output$RMSEvsKplot <- renderPlot({
    server_env$df()
    if (MEDSET_FLAG) {
      server_env$doKselectionPlot()
    }
  })

  ######################## Lambda selection main panel
  output$performancePanel <- renderUI({
    server_env$df()
    if (!is.null(input$performanceMode) &&
        !is.null(input$lambdaMin) && !is.null(input$lambdaMax)) {
      if (input$performanceMode == "lineplots") {
        list(
          plotOutput('lineplot',
                     height = "800px",
                     width = "600px"),
          br(),
          downloadLink("lineplotPDF", "PDF")
        )
      } else if (input$performanceMode == "table") {
         DT::dataTableOutput('performanceTable')
      }
    }
  })

  output$lineplot <- renderPlot({
    server_env$df()
    server_env$doLambdaPlot()
  })


  ######################## LMC main panel
  output$componentsPanel <- renderUI({
    server_env$df()
    K <- input$K_3
    if (!is.null(input$componentPlotType)) {
      if (input$componentPlotType == "mds plot" ||
          input$componentPlotType == "dendrogram") {
        h = "500px"
        w = "500px"
      } else if (input$componentPlotType == "heatmap") {
        h = "500px"
        w = sprintf("%dpx", max(500, 50 * as.integer(K)))
      } else if (input$componentPlotType == "scatterplot matching") {
        h0 = 300
        w0 = 300
        ncol = min(3, as.integer(K))
        nrow = (as.integer(K) %/% min(3, as.integer(K))) + as.integer(as.integer(K) %% min(3, as.integer(K)) >
                                                                        0)
        h = sprintf("%dpx", h0 * nrow)
        w = sprintf("%dpx", h0 * ncol)
      } else if (input$componentPlotType == "locus plot") {
        h = 500
        w = 1000
      } else{
        print("Hello")
        h0 = 300
        w0 = 300
        trueT <- server_env$getTrueT()
        ncol <- min(3, ncol(trueT))
        nrow <- min(2,
          ncol(trueT) %/% ncol + as.integer(ncol(trueT) %% ncol > 0))
        h = sprintf("%dpx", h0 * nrow)
        w = sprintf("%dpx", h0 * ncol)
        print(h)
        print(w)
      }
      print("Hello1")
      list(
        plotOutput('componentPlot',
                   height = h,
                   width =  w),
        br(),
        downloadLink("componentPlotPDF", "PDF")
      )
    }

  })

  output$componentPlot <- renderPlot({
    server_env$df()
    server_env$doComponentPlot()
  })

  ######################## Proportion Main Panel
  output$proportionplot <- renderPlot({
    server_env$df()
    if (!is.null(input$propPlotType)) {
      server_env$doProportionPlot()
    }
  })


  ######################## Meta-Analysis Panel

  output$metaAnalysisPanel <- renderUI({
    if (input$analysisType == "compare LMCs") {
      list(
        plotOutput(
          'comparisonPlot',
          height = if (input$comparativePlotType == "dendrogram")
            h
          else
            1.2 * h,
          width =  if (input$comparativePlotType == "dendrogram")
            2 * w
          else
            2.5 * w
        ),
        br()
      )
    } else if (input$analysisType == "differential methylation") {
      list(plotOutput('diffCGPlot'),
           if (input$diffOutputType == "Table") {
             DT::dataTableOutput('diffCGTable')
           }
           else if (input$diffOutputType == "GO Enrichments") {
             DT::dataTableOutput('goEnrichementTable')
           }
           else if (input$diffOutputType == "LOLA Enrichments") {
             DT::dataTableOutput('lolaEnrichementTable')
           }
           else{
             br()
           }
         )
    } else if (input$analysisType == "phenotype modeling") {
      if (input$phenoModelOutput == "summary plot") {
        list(plotOutput(
          'phenotypeModelPlot',
          height = h,
          width =  2 * w
        ),
        br())
      } else if (input$phenoModelOutput == "single case") {
        verbatimTextOutput("phenotypeModelSummary")
      }

    }

  })
  output$diffCGPlot<-renderPlot({
    server_env$doDiffCGPlot();
  })
  output$additionalMetaPlots<-renderUI({
    plotOutput("metaPlot")
    })
output$metaPlot<-renderPlot({
  server_env$doMetaPlot()
  })

  output$comparisonPlot <- renderPlot({
    server_env$doComparisonPlot()

  })
  output$phenotypeModelPlot<-renderPlot({
    server_env$doPhenotypeModelPlot();
  })


  #############################################
  ############# Sidebar outputs ###############
  #############################################

  ########################################### Sidebar outputs for K selection tab
  server_output_k_selec(input, output, server_env)

  ######################################## Sidebar outputs for lambda selection tab
  server_output_l_selec(input, output, server_env)

  #################################################### Sidebar output LMCs panel
  server_output_lmc(input,output, server_env)

  #################################################### Sidebar output Proportions panel
  server_output_proportion(input, output, server_env)

  ################################################Sidebar output Meta Analysis panel
  server_output_meta(input, output, server_env)
}
