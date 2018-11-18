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

server_env$getGOEnrichmenttable<-eventReactive(input$GOsubmitQuery, {
    showModal(modalDialog("Performing GO Enrichments, This may take upto 15 mins depending on the threshold
                            and number of LMC's", footer=NULL))
    results<-server_env$dataset()
    gr_list <- results@parameters$cg_subsets
    gr<-as.integer(input$cg_group_5)
    cg_ <- gr_list[gr]
    ll<-as.integer(input$lambda_5)
    lambdas <- results@parameters$lambdas
    lambda <- lambdas[ll]
    K<-input$K_5
    #finds out the index of k in Ks
    Ks<-results@parameters$Ks
    type="hypo"
    if(input$diffTableType=="hypermethylated"){
    type="hyper"
    }
    out<- tryCatch({
      MeDeCom::lmc.go.enrichment(results, anno.data=server_env$getCGAnnot(),
                                        K=K,
                                        lambda=lambda,
                                        cg_subset=as.integer(cg_),
                                        diff.threshold=input$dmr_threshold,
                                        region.type=input$region_type,
                                        temp.dir=tempdir(),
                                        type=type,
                                        assembly=input$assembly)
                                  }, error = function(err) {
                                    print(paste("MY_ERROR:  ",err))
                                  })
    removeModal()
    return(out)
  })
output$goEnrichementTable<-DT::renderDataTable({
  server_env$getGOEnrichmenttable()
  if(!is.null(input$lmc_go)){
    require(GOstats)
    return(summary(server_env$getGOEnrichmenttable()[[input$lmc_go]]))
  }else{
    return(data.frame())
  }

},rownames=FALSE, selection='single')

server_env$lmcgoSelect<-reactive({
  server_env$getGOEnrichmenttable()
  selectInput('lmc_go', 'LMC:', names(server_env$getGOEnrichmenttable()), selectize =
                TRUE)
  })
  server_env$lmclolaSelect<-reactive({
    server_env$getLOLAEnrichmenttable()
    selectInput('lmc_lola', 'LMC:', names(server_env$getLOLAEnrichmenttable()), selectize =
                  TRUE)
    })


server_env$getLOLAEnrichmenttable<-eventReactive(input$LOLAsubmitQuery, {
    showModal(modalDialog("Performing LOLA Enrichments, This may take upto 15 mins depending on the threshold
                            and number of LMC's", footer=NULL))
    results<-server_env$dataset()
    gr_list <- results@parameters$cg_subsets
    gr<-as.integer(input$cg_group_5)
    cg_ <- gr_list[gr]
    ll<-as.integer(input$lambda_5)
    lambdas <- results@parameters$lambdas
    lambda <- lambdas[ll]
    K<-input$K_5
    #finds out the index of k in Ks
    Ks<-results@parameters$Ks
    type="hypo"
    if(input$diffTableType=="hypermethylated"){
    type="hyper"
    }else if(input$diffTableType=="differential"){
      type="differential"
    }
    lola.db<-NULL
    loladb_path=NULL
    if (input$assembly=="hg38"){
       loladb_path="/home/reaper/epigen/GIT/lola_hg38.RData"
    }else if (input$assembly=="hg19"){
       loladb_path="/home/reaper/epigen/GIT/lola_hg19.RData"
    }else if (input$assembly=="mm10"){
       loladb_path="/home/reaper/epigen/GIT/lola_mm10.RData"
    }
    if(file.exists(loladb_path)){
      new.envi <- new.env()
      load(loladb_path, envir=new.envi)
      lola.db <- get(ls(envir = new.envi),envir = new.envi)
      new.envi <- new.env()
      gc()
    }else{
        lola.db<-MeDeCom::load.lola.for.medecom(dir.path=tempdir(), assembly=input$assembly)
    }
    out<- tryCatch({
      MeDeCom::lmc.lola.enrichment(results,
        annotation.filter=NULL,
        server_env$getCGAnnot(),
                                        K=K,
                                        lambda=lambda,
                                        cg_subset=as.integer(cg_),
                                        diff.threshold=input$dmr_threshold,
                                        region.type=input$region_type,
                                        temp.dir=tempdir(),
                                        type=type,
                                        assembly=input$assembly,
                                      lola.db=lola.db)
                                  }, error = function(err) {
                                    print(paste("MY_ERROR:  ",err))
                                  })
    removeModal()
    return(out)
  })
output$lolaEnrichementTable<-DT::renderDataTable({
  server_env$getLOLAEnrichmenttable()
  if(!is.null(input$lmc_lola)){
    return(server_env$getLOLAEnrichmenttable()[[input$lmc_lola]])
  }else{
    return(data.frame())
  }
})
}
