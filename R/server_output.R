#This file contains the output objects of the server

#############################################
########### main panel outputs ##############
#############################################

##########################################  Home tab main panel
server_output <- function(input, output, server_env) {
  output$files <- renderPrint({
      if(server_env$path()==""){
        print("Please Select or provide path to a directory")
      }else{
        path<-server_env$path()
        pattern<-sub('.*\\/', '', path)
        path<-sub("/[^/]+$", "", path)
        list.files(path, pattern=pattern)
      }
    })

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
    if (MEDSET_FLAG) {
      server_env$doKselectionPlot()
    }
  })

  ######################## Lambda selection main panel
  output$performancePanel <- renderUI({
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
    server_env$doLambdaPlot()
  })


  ######################## LMC main panel
  output$componentsPanel <- renderUI({
    K <- input$K_3
    if (!is.null(input$componentPlotType)) {
      if (input$componentPlotType == "mds plot" ||
          input$componentPlotType == "dendrogram") {
        h = "500px"
        w = "500px"
      } else if (input$componentPlotType == "heatmap") {
        h = "500px"
        w = sprintf("%dpx", max(500, 50 * as.integer(K)))
      } else if (input$componentPlotType %in% c("scatterplot all", "scatterplot matching", "scatterplot avg matching")) {
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
        nrow <- min(2,
          ncol(trueT) %/% ncol + as.integer(ncol(trueT) %% ncol > 0))
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
    server_env$doComponentPlot()
  })

  ######################## Proportion Main Panel
  output$proportionplot <- renderPlot({
    if (!is.null(input$propPlotType)) {
      server_env$doProportionPlot()
    }
  })


  ######################## Meta-Analysis Panel

  output$metaAnalysisPanel <- renderUI({
    if(!is.null(input$analysisType)){
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
           downloadLink("diffCGPlotPDF", "PDF"),
           DT::dataTableOutput('diffCGTable')
          )
      }
      else if((input$analysisType == "Enrichments")){
        if (input$diffOutputType == "GO Enrichments"){
          DT::dataTableOutput('goEnrichementTable')
        }else if (input$diffOutputType == "LOLA Enrichments") {
          list(plotOutput("metaPlot"),
          DT::dataTableOutput('lolaEnrichementTable'),
          if(!is.na(server_env$getLOLAEnrichmenttable()[[input$lmc_lola]])){
            downloadLink("metaPlotPDF", "Lola Plot PDF")
          }else{
            br()
          })

         }else{
           br()
         }
      } else if(input$analysisType=="Trait Association"){
        list(plotOutput('TraitAssociation'),
             downloadLink("TraitAssociationPDF", "PDF"))
        }else{
        br()
      }
    }
  })


  output$downloadPanel<-renderUI({
				wellPanel(
						h4("Input Data"),
						{sprintf("Target data (intensities) for CpG subset # %s",input$cg_group_6)},
						downloadLink('inputDataIntMat', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("Target data (ratios) for CpG subset # %s",input$cg_group_6)},
						downloadLink('inputDataMat', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("CpG subset # %s (with respect to the RnBeads HM450 annotation)",input$cg_group_6)},
						downloadLink('inputCGsubset', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("Sample information")},
						downloadLink('inputPheno', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("Sample subset")},
						downloadLink('inputSampleSubset', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("Reference profiles (T^star) for CpG subset # %s ",input$cg_group_6)},
						downloadLink('inputRefDataMat', label = "(.MAT)", class = NULL),
						br(),
						if(!is.null(server_env$getTrueA())) {sprintf("Known proportion matrix (A^star)")},
						if(!is.null(server_env$getTrueA())) downloadLink('inputTrueA',label="(.MAT)"),
						hr(),
						h4("Results"),
						{ sprintf("Results for CpG subset # %s , k %s, lambda %g",
					    input$cg_group_6, input$K_6, server_env$dataset()@parameters$lambdas[as.integer(input$lambda_6)])},
						downloadLink('outputResults', label = "(.MAT)", class = NULL)
				)

			})
      output$inputDataIntMat<-downloadHandler(
  			filename=function(){
  				gr_lists<-server_env$getRuns()[[input$analysisrun]][["cg_subsets"]]
  				group_names<-sapply(gr_lists, paste, collapse="_")
  				sprintf("input_data_M_U_cggroup_%s.mat",group_names[as.integer(input$cg_group)])
  			},
  			content=function(con){
  				ind<-getCGsubset()
  				#writeMat(con, D=getMethData()[ind,])
  				system(sprintf("cp %s %s", file.path(getRuns()[[input$analysisrun]][["data.dir"]], "MandU.mat") ,con))
  			}
  	)

  output$TraitAssociation<-renderPlot({
    server_env$doTraitAssociation()
  })

  output$diffCGPlot<-renderPlot({
    server_env$doDiffCGPlot();
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

  ################################################Sidebar output Downloads panel
  server_output_downloads(input, output, server_env)
}

server_tab_uniformity_keeper<-function(input, output, session, server_env){
  ###################### Handle CpG Subset
  observeEvent(input$cg_group, server_env$Selected$CG<-input$cg_group)
  observeEvent(input$cg_group_2, server_env$Selected$CG<-input$cg_group_2)
  observeEvent(input$cg_group_3, server_env$Selected$CG<-input$cg_group_3)
  observeEvent(input$cg_group_4, server_env$Selected$CG<-input$cg_group_4)
  observeEvent(input$cg_group_5, server_env$Selected$CG<-input$cg_group_5)
  observeEvent(input$cg_group_6, server_env$Selected$CG<-input$cg_group_6)

  observeEvent(server_env$Selected$CG, updateSelectInput(session, "cg_group", selected=server_env$Selected$CG))
  observeEvent(server_env$Selected$CG, updateSelectInput(session, "cg_group_2", selected=server_env$Selected$CG))
  observeEvent(server_env$Selected$CG, updateSelectInput(session, "cg_group_3", selected=server_env$Selected$CG))
  observeEvent(server_env$Selected$CG, updateSelectInput(session, "cg_group_4", selected=server_env$Selected$CG))
  observeEvent(server_env$Selected$CG, updateSelectInput(session, "cg_group_5", selected=server_env$Selected$CG))
  observeEvent(server_env$Selected$CG, updateSelectInput(session, "cg_group_6", selected=server_env$Selected$CG))


  ######################### Handle Lambdas
  observeEvent(input$lambda, server_env$Selected$LAMBDA<-input$lambda)
  observeEvent(input$lambda_3, server_env$Selected$LAMBDA<-input$lambda_3)
  observeEvent(input$lambda_4, server_env$Selected$LAMBDA<-input$lambda_4)
  observeEvent(input$lambda_5, server_env$Selected$LAMBDA<-input$lambda_5)
  observeEvent(input$lambda_6, server_env$Selected$LAMBDA<-input$lambda_6)

  observeEvent(server_env$Selected$LAMBDA, updateSelectInput(session, "lambda", selected=server_env$Selected$LAMBDA))
  observeEvent(server_env$Selected$LAMBDA, updateSelectInput(session, "lambda_3", selected=server_env$Selected$LAMBDA))
  observeEvent(server_env$Selected$LAMBDA, updateSelectInput(session, "lambda_4", selected=server_env$Selected$LAMBDA))
  observeEvent(server_env$Selected$LAMBDA, updateSelectInput(session, "lambda_5", selected=server_env$Selected$LAMBDA))
  observeEvent(server_env$Selected$LAMBDA, updateSelectInput(session, "lambda_6", selected=server_env$Selected$LAMBDA))

  ######################### Handle Ks
  observeEvent(input$K_2, server_env$Selected$K<-input$K_2)
  observeEvent(input$K_3, server_env$Selected$K<-input$K_3)
  observeEvent(input$K_4, server_env$Selected$K<-input$K_4)
  observeEvent(input$K_5, server_env$Selected$K<-input$K_5)
  observeEvent(input$K_6, server_env$Selected$K<-input$K_6)

  observeEvent(server_env$Selected$K, updateSelectInput(session, "K_2", selected=server_env$Selected$K))
  observeEvent(server_env$Selected$K, updateSelectInput(session, "K_3", selected=server_env$Selected$K))
  observeEvent(server_env$Selected$K, updateSelectInput(session, "K_4", selected=server_env$Selected$K))
  observeEvent(server_env$Selected$K, updateSelectInput(session, "K_5", selected=server_env$Selected$K))
  observeEvent(server_env$Selected$K, updateSelectInput(session, "K_6", selected=server_env$Selected$K))
}
