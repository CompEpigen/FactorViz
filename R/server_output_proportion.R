server_output_proportion <- function(input, output, server_env) {
  output$groupSelector_4 <- renderUI({
    if (MEDSET_FLAG) {
      results <- server_env$dataset()
      gr_lists <- results@parameters$cg_subsets
      GROUPS <- 1:length(gr_lists)
      if (is.null(names(gr_lists))) {
        names(GROUPS) <- sapply(gr_lists, paste, collapse = "_")
      } else{
        names(GROUPS) <- names(gr_lists)
      }
      selectInput('cg_group_4', 'Technical CpG subset:', GROUPS, selectize =
                    TRUE)
    }
  })

  output$Kselector_4 <- renderUI({
    Ks <- server_env$dataset()@parameters$Ks
    selectInput('K_4', 'Number of LMCs (k)', Ks, selectize = TRUE)
  })

  output$lambdaSelector_4 <- renderUI({
    if (MEDSET_FLAG) {
      LAMBDAS <- server_env$dataset()@parameters$lambdas
      LAMBDA.IDS <- 1:length(server_env$dataset()@parameters$lambdas)
      names(LAMBDA.IDS) <- as.character(LAMBDAS)
      selectInput('lambda_4', 'Lambda value', LAMBDA.IDS)
    }

  })

  output$propPlotT <- renderUI({
    server_env$df()
    p_measure <-
      c(
        "heatmap",
        "barplot",
        "lineplot",
        "correlations"
      )
      if(TRUE_A_FLAG){
          p_measure<-c(p_measure, "scatterplot")
      }
    selectInput('propPlotType', 'Plot type',
                p_measure,
                selected = 1)
  })

  output$propMatrixSelector <- renderUI({
    if (!is.null(input$propPlotType)) {
      if (input$propPlotType == "scatterplot") {
        prop_mats <- c("regression")
        labl <- "Reference proportions:"
      } else{
        prop_mats <- c("MeDeCom", "regression")
        labl <- "Proportions:"
      }
      if ("analysisrun_ref" %in% names(input)) {
        prop_mats <- c(prop_mats, "regression ref cmp")
      }
      selectInput('propMatrix', labl, prop_mats, selectize = TRUE)
    }

  })

  output$componentSelectorRef <- renderUI({
    comps <- c(1:input$K_ref)
    selectInput(
      'component_ref',
      'LMC (ref analysis):',
      comps,
      selectize = TRUE,
      multiple = TRUE,
      selected = isolate({
        if ("component_ref" %in% names(input))
          as.character(input$component_ref)
        else
          1
      })
    )
  })

  output$componentSelector_4 <- renderUI({
    if(!is.null(input$K_4)){
    comps <- c(1:input$K_4, NA)
    names(comps) <- c(as.character(1:input$K_4), "sum best matching")
    }else{
      comps <- c(NA)
      names(comps) <- c("sum best matching")
    }
    selectInput(
      'component_4',
      'LMC:',
      comps,
      selectize = TRUE,
      multiple = TRUE,
      selected = 1
    )
  })

  output$refProfileSelector <- renderUI({
    rprofiles <- c()
    rprofile_names <- c()
    if (!is.null(server_env$getTrueT())) {
      rprofiles <- c(rprofiles, 1:ncol(server_env$getTrueT()))
      rprofile_names_add <- colnames(server_env$getTrueT())
      if (is.null(rprofile_names_add)) {
        rprofile_names_add <- as.character(1:ncol(server_env$getTrueT()))
      }
      rprofile_names <- c(rprofile_names, rprofile_names_add)
    }
    names(rprofiles) <- rprofile_names
    selectInput('profile', 'Reference Profile:', rprofiles, selectize = TRUE)

  })

  output$sampleColorSelector_4 <- renderUI({
      if(PHENO_DATA_FLAG){
      pd <- server_env$getPhenoData()
      if (!is.null(pd)) {
        siteannot <- colnames(pd)
      } else{
        siteannot <- character()
      }
      selectInput('mdsDataCat_4',
                  'Color samples by:',
                  c("none", siteannot),
                  selectize = TRUE)
    }
    })

}
