server_output_l_selec<-function(input, output, server_env){
output$groupSelector_2 <- renderUI({
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
    selectInput('cg_group_2', 'Technical CpG subset:', GROUPS, selectize =
                  TRUE)
  }
})

output$Kselector_2 <- renderUI({
  server_env$df()
  #Ks<-dataset()[["Ks"]]
  Ks <- server_env$dataset()@parameters$Ks
  selectInput('K_2', 'Number of LMCs (k)', Ks, selectize = TRUE)
})

output$minLambdaSelector <- renderUI({
  server_env$df()
  lambda_list <- sort(server_env$dataset()@parameters$lambdas)
  selectInput('lambdaMin', 'Minimum lambda:', lambda_list, selected = lambda_list[which.min(lambda_list)])
})

output$maxLambdaSelector <- renderUI({
  server_env$df()
  lambda_list <- sort(server_env$dataset()@parameters$lambdas)
  selectInput('lambdaMax', 'Maximum lambda:', lambda_list, selected = lambda_list[which.max(lambda_list)])
})

output$performanceModeSelector <- renderUI({
  server_env$df()
  selectInput('performanceMode',
              'Show results as:',
              c("lineplots", "table"),
              selectize = TRUE)
})

}