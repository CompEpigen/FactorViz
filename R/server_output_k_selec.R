server_output_k_selec <- function(input, output, server_env) {
  output$groupSelector <- renderUI({
    server_env$df()
    if (MEDSET_FLAG) {
      results <- server_env$dataset()
      gr_lists <- results@parameters$cg_subsets
      GROUPS <- 1:length(gr_lists)
      if (is.null(names(gr_lists))) {
        names(GROUPS) <- sapply(gr_lists, paste, collapse = "_")
      } else{
        names(GROUPS) <- names(gr_lists)
      }
      selectInput('cg_group', 'Technical CpG subset:', GROUPS, selectize = TRUE)
    }
  })
  
  output$minKselector <- renderUI({
    server_env$df()
    if (MEDSET_FLAG) {
      selectInput('minK',
                  'Minimum k:',
                  server_env$dataset()@parameters$Ks)
    }
  })
  
  output$maxKselector <- renderUI({
    server_env$df()
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
    server_env$df()
    if (MEDSET_FLAG) {
      LAMBDAS <- server_env$dataset()@parameters$lambdas
      LAMBDA.IDS <-
        1:length(server_env$dataset()@parameters$lambdas)
      names(LAMBDA.IDS) <- as.character(LAMBDAS)
      selectInput('lambda', 'Lambda value', LAMBDA.IDS)
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
    selectInput("KvsStat", "Statistic to plot:",
                p_measure,
                selected = 2)
  })
}