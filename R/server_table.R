server_table<-function(input, output, server_env){

##################LAMBDA SELECTION TAB
    output$performanceTable <- DT::renderDataTable({
      server_env$df()
      results <- server_env$dataset()
      gr <- as.integer(input$cg_group_2)
      elts <- PERFORMANCE_MEASURES
      output <- list()
      test_Ks <- input$K_2
      Ks <- results@parameters$Ks
      index <- match(test_Ks, Ks)
      for (elt in 1:length(elts)) {
        min_lls <- integer(length(test_Ks))
        min_vals <- double(length(test_Ks))
        if (all(is.na(results@outputs[[gr]][[elts[elt]]]))) {
          next
        }
        for (kk in 1:length(test_Ks)) {
          min_lls[kk] <-
            which.min(results@outputs[[gr]][[elts[elt]]][index, , drop = F])
          min_vals[kk] <-
            results@outputs[[gr]][[elts[elt]]][index, min_lls[kk], drop = F]
        }
        min_kk <- which.min(min_vals)
        output[[elt]] <-
          c(
            names(elts)[elt],
            min(min_vals[min_kk]),
            results@parameters$lambdas[min_lls[min_kk]]
          )
        if (length(test_Ks) > 1) {
          output[[elt]] <- c(output[[elt]], sprintf("%d", test_Ks[kk]))
        }
      }
      output <- do.call("rbind", output)
      rownames(output) <- NULL
      cn <- c("Statistic", "Minimal value", "Lambda")
      if (length(test_Ks) > 1) {
        cn <- c(cn, "K")
      }
      colnames(output) <- cn
      output
    }, rownames = FALSE)


#################META ANALYSIS TAB
output$diffCGTable<-DT::renderDataTable({

  diff_cgs<-server_env$getDiffCGs()
  if(!is.null(input$diffTableType)){
  if(input$diffTableType=="hypomethylated"){
    output<-diff_cgs[[1]]
  }else if(input$diffTableType=="hypermethylated"){
    output<-diff_cgs[[2]]
  }

  output
  }
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
    result<-server_env$getGOEnrichmenttable()[[input$lmc_go]]
    if(is.na(result)){
      result<-data.frame(
        GOBPID = character(),
        Pvalue = integer(),
        OddsRatio = numeric(),
        ExpCount=numeric(),
        Count=numeric(),
        Size=numeric(),
        Term=character(),
        p.val.adj.fdr=numeric()
      )
    }else{
    numVars<-sapply(result, is.numeric)
    result[numVars] <- lapply(result[numVars], round, digits = 2)
    return(result)
  }
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
      lola.db <<- get(ls(envir = new.envi),envir = new.envi)
      new.envi <- new.env()
      gc()
    }else{
        lola.db<<-MeDeCom::load.lola.for.medecom(dir.path=tempdir(), assembly=input$assembly)
    }
    print(lola.db)
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
    result<-server_env$getLOLAEnrichmenttable()[[input$lmc_lola]]
    if(is.na(result)){
      result<-data.frame(
        dbSet = character(),
        collection = integer(),
        pValueLog = numeric(),
        oddsRatio=numeric(),
        description=character(),
        cellType=character(),
        qValue=numeric()
      )
    }else{
    numVars<-sapply(result, is.numeric)
    result[numVars] <- lapply(result[numVars], round, digits = 2)
    numVars<-names(result)
    selected<-c('dbSet','collection','pValueLog', 'oddsRatio', 'description', 'cellType', 'qValue')
    result$description <- gsub(x = result$description, pattern = ";", replace = ", ")
    result<-result[, selected]
    }
    return(result)
  }else{
    return(data.frame())
  }
})
}
