server_output_lmc <- function(input, output, server_env) {
  output$groupSelector_3 <- renderUI({
    if (MEDSET_FLAG) {
      results <- server_env$dataset()
      gr_lists <- results@parameters$cg_subsets
      GROUPS <- 1:length(gr_lists)
      if (is.null(names(gr_lists))) {
        names(GROUPS) <- sapply(gr_lists, paste, collapse = "_")
      } else{
        names(GROUPS) <- names(gr_lists)
      }
      selectInput('cg_group_3', 'Technical CpG subset:', GROUPS,selected=server_env$Selected$CG)
    }
  })

  output$Kselector_3 <- renderUI({
    Ks <- server_env$dataset()@parameters$Ks
    selectInput('K_3', 'Number of LMCs (k)', Ks, selected=server_env$Selected$K)
  })

  output$lambdaSelector_3 <- renderUI({
    if (MEDSET_FLAG) {
      LAMBDAS <- server_env$dataset()@parameters$lambdas
      LAMBDA.IDS <- 1:length(server_env$dataset()@parameters$lambdas)
      names(LAMBDA.IDS) <- as.character(LAMBDAS)
      selectInput('lambda_3', 'Lambda value', LAMBDA.IDS, selected=server_env$Selected$LAMBDA)
    }

  })

  output$componentPlotT <- renderUI({
    p_type <-
      c(
        "dendrogram",
        "extremality",
        "heatmap",
        "similarity graph"
      )
    if(TRUE_T_FLAG && (!is.null(input$useReferences) && (input$useReferences))){
      p_type<-c(p_type, "scatterplot all",
              "scatterplot matching",
              "scatterplot avg matching")
    }
    if (!is.null(input$K_3) && (as.integer(input$K_3) > 2)){
      p_type<-c(p_type, "mds plot")
    }
    if (METH_DATA_FLAG && !is.null(input$useReferences) && (input$useReferences)) {
      p_type <- c(p_type, "distance to center")

    }
    selectInput('componentPlotType', 'Plot type',
                p_type,
                selected = 1)

  })

  output$componentSelector <- renderUI({

    comps <- c(1:input$K_3, NA)
    names(comps) <- c(as.character(1:input$K_3), "sum best matching")
    selectInput(
      'component',
      'LMC:',
      comps,
      selectize = TRUE,
      multiple = TRUE,
      selected = 1
    )
  })
  output$cgVar<-renderUI({
   if(TRUE_T_FLAG){
     checkboxInput("cgVarSubset", "Select top-variable CpGs:", value = FALSE)
   }
  })

  output$topSDcgsSelector <- renderUI({


    gr <- as.integer(input$cg_group_3)
    ind <- server_env$getCGsubset_3()
    sliderInput(
      'topSDcpgs',
      'Select top SD cgs',
      min = 1,
      max = length(ind),
      value = length(ind),
      step = 500,
      round = 0
    )

  })

  output$sampleColorSelector <- renderUI({

    pd <- server_env$getPhenoData()
    if (!is.null(pd)) {
      siteannot <- colnames(pd)
    } else{
      siteannot <- character()
    }
    selectInput('mdsDataCat',
                'Color samples by:',
                c("none", siteannot),
                selectize = TRUE)
  })

  output$pointColorSelector <- renderUI({

    cats <- names(server_env$getCGcategories())
    feats <- names(server_env$getCGquantFeatureSettings())
    selectInput('pointCategory',
                'Color data points by:',
                c("none", cats, feats),
                selectize = TRUE)
  })

  output$pointFilter <- renderUI({

    if ("pointCategory" %in% names(input) &&
        input$pointCategory != "none") {
      if (input$pointCategory %in% names(server_env$getCGcategories())) {
        list(checkboxInput('CGsubsetToCategory', "limit to", value = FALSE))
      } else if (input$pointCategory %in% names(server_env$getCGquantFeatureSettings())) {
        features <- server_env$getCGquantFeatureSettings()
        list(
          sliderInput(
            'quantFilterMin',
            "Minimal value:",
            min = features[[input$pointCategory]]$min,
            max = features[[input$pointCategory]]$max,
            value = features[[input$pointCategory]]$min
          ),
          sliderInput(
            'quantFilterMax',
            "Maximal value:",
            min = features[[input$pointCategory]]$min,
            max = features[[input$pointCategory]]$max,
            value = features[[input$pointCategory]]$max
          )
        )
      }
    }
  })

  output$CGcategorySubsetSelector <- renderUI({

    if ("CGsubsetToCategory" %in% names(input) &&
        input$CGsubsetToCategory && input$pointCategory != "none") {
      cats <- getCGcategories()
      features <- 1:length(cats[[input$pointCategory]])
      names(features) <- cats[[input$pointCategory]]
      selectInput(
        'pointSubset',
        'Show only:',
        features,
        selectize = TRUE,
        multiple = TRUE,
        selected = 1
      )
    }
  })

  output$geneSetSelector <- renderUI({

    gene.sets <- getGeneSets()
    selectInput('geneSet', 'Gene set:', names(gene.sets), selectize = TRUE)
  })

  output$locusSelector <- renderUI({

    gene.sets <- getGeneSets()
    selectInput('locus', 'Gene:', gene.sets[[input$geneSet]], selectize =
                  TRUE)
  })

}
