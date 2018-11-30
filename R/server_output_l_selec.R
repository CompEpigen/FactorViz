server_output_l_selec<-function(input, output, server_env){
output$groupSelector_2 <- renderUI({

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

  Ks <- server_env$dataset()@parameters$Ks
  selectInput('K_2', 'Number of LMCs (k)', Ks, selectize = TRUE)
})

output$minLambdaSelector <- renderUI({

  lambda_list <- sort(server_env$dataset()@parameters$lambdas)
  selectInput('lambdaMin', 'Minimum lambda:', lambda_list, selected = lambda_list[which.min(lambda_list)])
})

output$maxLambdaSelector <- renderUI({

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
output$includeStats<-renderUI({
  results<-server_env$dataset()
  out<-c()
  if(!any(sapply(list(input$K_2, input$lambdaMax, input$lambdaMin, input$cg_group_2), is.null))){
    llsubs<-results@parameters$lambdas >= as.numeric(input$lambdaMin) & results@parameters$lambdas <= as.numeric(input$lambdaMax)
    lmbd<-results@parameters$lambdas[llsubs]
    gr_list <- results@parameters$cg_subsets
    gr<-as.integer(input$cg_group_2)
    cg_ <- gr_list[gr]
    if (!is.na(MeDeCom:::getStatistics(results, input$K_2, lmbd, cg_, statistic="rmseT")) ||
    length(MeDeCom:::getStatistics(results, input$K_2, lmbd, cg_, statistic="rmseT"))<1){
        out<-c(out, checkboxInput("includeRMSE_T", "Include RMSE of T", value=FALSE))
    }
    if (!is.na(MeDeCom:::getStatistics(results, input$K_2, lmbd, cg_, statistic="dist2C")) ||
    length(MeDeCom:::getStatistics(results, input$K_2, lmbd, cg_, statistic="dist2C"))<1){
        output<-c(out, checkboxInput("includeDist2C_T", "Include MDC of T", value=FALSE))
      }
      if (!is.na(MeDeCom:::getStatistics(results, input$K_2, lmbd, cg_, statistic="maeA")) ||
      length(MeDeCom:::getStatistics(results, input$K_2, lmbd, cg_, statistic="maeA"))<1){
          output<-c(out, checkboxInput("includeMAE_A", "Include MAE of A", value=FALSE))
        }
  }
  out<-list(out)
  out
  })
}
