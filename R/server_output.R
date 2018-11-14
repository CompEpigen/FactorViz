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
        tableOutput('performanceTable')
      }
    }
  })

  output$lineplot <- renderPlot({
    server_env$df()
    server_env$doLambdaPlot()
  })

  output$performanceTable <- renderTable({
    server_env$df()
    results <- server_env$dataset()
    gr <- as.integer(input$cg_group_2)
    elts <- PERFORMANCE_MEASURES
    output <- list()
    test_Ks <- input$K_2
    Ks <- results@parameters$Ks
    index <- match(test_Ks, Ks)
    for (elt in 1:length(elts)) {
      min_lls <- integer(length(test_Ks))
      min_vals <- double(length(test_Ks))
      if (all(is.na(results@outputs[[gr]][[elts[elt]]]))) {
        next
      }
      for (kk in 1:length(test_Ks)) {
        #use the index instead of test_Ks
        min_lls[kk] <-
          which.min(results@outputs[[gr]][[elts[elt]]][index, , drop = F])
        min_vals[kk] <-
          results@outputs[[gr]][[elts[elt]]][index, min_lls[kk], drop = F]
      }
      min_kk <- which.min(min_vals)
      output[[elt]] <-
        c(
          names(elts)[elt],
          sprintf("%f", min(min_vals[min_kk])),
          sprintf("%f", results@parameters$lambdas[min_lls[min_kk]])
        )
      if (length(test_Ks) > 1) {
        output[[elt]] <- c(output[[elt]], sprintf("%d", test_Ks[kk]))
      }
    }
    output <- do.call("rbind", output)
    rownames(output) <- NULL
    cn <- c("Statistic", "Minimal value", "Lambda")
    if (length(test_Ks) > 1) {
      cn <- c(cn, "K")
    }
    colnames(output) <- cn
    output
  }, include.rownames = FALSE)

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
        h0 = 300
        w0 = 300
        trueT <- server_env$getTrueT()
        ncol <- min(3, ncol(trueT))
        nrow <-
          ncol(trueT) %/% ncol + as.integer(ncol(trueT) %% ncol > 0)
        h = sprintf("%dpx", h0 * nrow)
        w = sprintf("%dpx", h0 * ncol)
      }
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
    w = 500
    h = 500
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
