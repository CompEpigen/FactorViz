server_table<-function(input, output, server_env){
output$diffCGTable<-DT::renderDataTable({

  diff_cgs<-server_env$getDiffCGs()

  if(input$diffTableType=="hypomethylated"){
    output<-diff_cgs[[1]]
  }else if(input$diffTableType=="hypermethylated"){
    output<-diff_cgs[[2]]
  }

  output
},rownames=FALSE, selection = 'single')

output$goEnrichementTable<-DT::renderDataTable({
  results<-server_env$dataset()
  gr<-as.integer(input$cg_group_5)
  ll<-as.integer(input$lambda_5)
  K<-input$K_5
  #finds out the index of k in Ks
  Ks<-results@parameters$Ks
  type="hypo"
  if(input$diffTableType=="hypermethylated"){
  type="hyper"
  }

  out<-MeDeCom::lmc.go.enrichment(results, anno.data=server_env$getCGAnnot(),
                                    K=K,
                                    lambda=ll,
                                    cg_subset=server_env$getCGsubset_5(),
                                    diff.threshold=input$dmr_threshold,
                                    region.type="genes",
                                    temp.dir=tempdir(),
                                    type=type,
                                    assembly="hg19")
},rownames=FALSE, selection='single')
}
