server_output_downloads <- function(input, output, server_env) {
  output$groupSelector_6 <- renderUI({

    if (MEDSET_FLAG) {
      results <- server_env$dataset()
      gr_lists <- results@parameters$cg_subsets
      GROUPS <- 1:length(gr_lists)
      if (is.null(names(gr_lists))) {
        names(GROUPS) <- sapply(gr_lists, paste, collapse = "_")
      } else{
        names(GROUPS) <- names(gr_lists)
      }
      selectInput('cg_group_3', 'Technical CpG subset:', GROUPS, selectize =
                    TRUE)
    }
  })

  output$Kselector_6 <- renderUI({

    #Ks<-dataset()[["Ks"]]
    Ks <- server_env$dataset()@parameters$Ks
    selectInput('K_3', 'Number of LMCs (k)', Ks, selectize = TRUE)
  })

  output$lambdaSelector_6 <- renderUI({
    
    if (MEDSET_FLAG) {
      LAMBDAS <- server_env$dataset()@parameters$lambdas
      LAMBDA.IDS <- 1:length(server_env$dataset()@parameters$lambdas)
      names(LAMBDA.IDS) <- as.character(LAMBDAS)
      selectInput('lambda_3', 'Lambda value', LAMBDA.IDS)
    }

  })
}
