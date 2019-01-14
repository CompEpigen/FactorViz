server_output_k_selec <- function(input, output, server_env) {
  output$groupSelector <- renderUI({
    if (MEDSET_FLAG) {
      results <- server_env$dataset()
      gr_lists <- results@parameters$cg_subsets
      GROUPS <- 1:length(gr_lists)
      if (is.null(names(gr_lists))) {
        names(GROUPS) <- sapply(gr_lists, paste, collapse = "_")
      } else{
        names(GROUPS) <- names(gr_lists)
      }
      selectInput('cg_group', 'Technical CpG subset:', GROUPS, selected=server_env$Selected$CG)
    }
  })

  output$minKselector <- renderUI({
    if (MEDSET_FLAG) {
      selectInput('minK',
                  'Minimum k:',
                  server_env$dataset()@parameters$Ks)
    }
  })

  output$maxKselector <- renderUI({
    if (MEDSET_FLAG) {
      selectInput(
        'maxK',
        'Maximum k:',
        server_env$dataset()@parameters$Ks,
        selected = server_env$dataset()@parameters$Ks[length(server_env$dataset()@parameters$Ks)]
      )
    }
  })

  output$lambdaSelector <- renderUI({
    if (MEDSET_FLAG) {
      if (is.null(input$RMSEvsKplotAllLambdas)|| !input$RMSEvsKplotAllLambdas){
      LAMBDAS <- server_env$dataset()@parameters$lambdas
      LAMBDA.IDS <-
        1:length(server_env$dataset()@parameters$lambdas)
      names(LAMBDA.IDS) <- as.character(LAMBDAS)
      selectInput('lambda', 'Lambda value', LAMBDA.IDS, selected=server_env$Selected$LAMBDA)
    }
  }

  })

  output$KvsStat <- renderUI({
    server_env$df()
    p_measure <- c("Objective" = "Fval", "CV error" = "cve")
    if (METH_DATA_FLAG) {
      p_measure <-
        c(
          p_measure,
          "RMSE" = "rmse",
          "RMSE, T" = "rmseT",
          "MDC, T" = "dist2C",
          "MAE, A" = "maeA"
        )
    }
    if(DEVIANCE_FLAG){
      p_measure <- c(p_measure,"Deviance"="deviance")
    }
    selectInput("KvsStat", "Statistic to plot:",
                p_measure,
                selected = 2)
  })
}
