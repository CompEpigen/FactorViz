server_plot<-function(input, output, server_env){
server_env$doKselectionPlot<-function(){
  server_env$df()
  results<-server_env$dataset()
  statistic<-input$KvsStat
  Kvals<-results@parameters$Ks
  if("minK" %in% names(input) && "maxK" %in% names(input)){
    Kvals<-Kvals[Kvals>=as.integer(input$minK) & Kvals<=as.integer(input$maxK)]
  }

  #cg subset
  gr_list <- results@parameters$cg_subsets
  gr<-as.integer(input$cg_group)
  cg_ <- gr_list[gr]

  #lambdas
  ll <- as.integer(input$lambda)
  lambdas <- results@parameters$lambdas
  lambda <- lambdas[ll]
  if(input$RMSEvsKplotAllLambdas){
    lambda<-lambdas
  }
  sample_subset = NULL
  if(!is.null(statistic)){
  if(statistic=="rmse"){
    meth.data<-server_env$getMethData()
    sample_subset<-1:ncol(meth.data)
  }
  MeDeCom:::plot.K.selection(results, statistic = statistic, Ks = as.numeric(Kvals),lambdas = as.numeric(lambda), cg_subset = as.integer(cg_), sample_subset = sample_subset,cg_subsets = gr_list, KvsRMSElambdaLegend = TRUE, normalizedCVE = input$normalizedCVE, addPlotTitle = input$addPlotTitle)
  }
}

server_env$doLambdaPlot<-function(){
  results<-server_env$dataset()
  #cg subset
  gr_list <- results@parameters$cg_subsets
  gr<-as.integer(input$cg_group_2)
  k<-as.integer(input$K_2)
  cg_ <- gr_list[gr]
  #minLambda and maxLambda
  minLam = as.numeric(input$lambdaMin)
  maxLam = as.numeric(input$lambdaMax)
  scale = input$lambdaScale
  if (scale == "logarithmic"){
    scale = "log"
  }
  MeDeCom:::plot.lambda.selection(results, cg_subset = as.integer(cg_), K = k, minLambda = minLam, maxLambda =maxLam, scale= scale ,includeRMSE_T= input$includeRMSE_T, includeMAE_A = input$includeMAE_A, includeDist2C_T = input$includeDist2C_T )

}

server_env$doComponentPlot<-function(){
  results<-server_env$dataset()
  cmp<-as.integer(input$component)
  #cg subset
  gr_list <- results@parameters$cg_subsets
  gr<-as.integer(input$cg_group_3)
  cg_ <- gr_list[gr]

  #lambdas
  ll <- as.integer(input$lambda_3)
  lambdas <- results@parameters$lambdas
  lambda <- lambdas[ll]

  #Ks
  K <- as.integer(input$K_3)
  Ks <- results@parameters$Ks

  index<-match(K, Ks)

  ind<-server_env$getCGsubset_3()
  trueT<-server_env$getTrueT()
  Tref<-trueT[ind,]
  #types
  type <- input$componentPlotType
  if (type == "mds plot"){
    type <- "MDS"
  }
  #that
  That<-results@outputs[[as.character(gr)]]$T[[index,ll]]


  #sample.characteristic
  if(type=="MDS"){
    if(input$mdsDataCat!="none"){
      pheno.data<-server_env$getPhenoData()
      data.ch<-pheno.data[[input$mdsDataCat]]
    }
  }
  else{
    data.ch<-NULL
  }
  #top.cgs never used ?
  top.cgs <- NA
  if(type %in% c("heatmap","dendrogram","MDS","similarity graph")){
    if(input$cgVarSubset){
      top.cgs<-server_env$getTTandTrefSDranking()[1:input$topSDcpgs]
    }else if(!is.null(Tref)){
      top.cgs<-1:nrow(Tref)
    }else{
      top.cgs<-1:nrow(That)
    }

  }

  #scatterplot
  if(type %in% c("scatterplot matching","scatterplot all","scatterplot avg matching")){
    type <- "scatterplot"
  }

  if (type == "scatterplot matching"){
    scatter.matching <- TRUE
  }else{
    scatter.matching <-FALSE
  }
  if (input$scatterType =="smoothed"){
    scatter.smooth <- TRUE
  }else{
    scatter.smooth <- FALSE
  }

  if(type == "locus plot"){
    meth.data<-server_env$getMethData()
    sample_subset<-server_env$getSampleSubset()
    if(F){
    if(input$locusChr!="" && input$locusStart!="" && input$locusEnd!=""){
      locus_params<-list()
      locus_params[["ann"]]<-server_env$getCGAnnot()
      locus_params[["ann.genes"]]<-server_env$getGeneAnnot()
      locus_params[["locus.name"]]=input$locusName
      locus_params[["locus.chr"]]=input$locusChr
      locus_params[["locus.start"]]=as.integer(input$locusStart)
      locus_params[["locus.end"]]=as.integer(input$locusEnd)
      locus_params[["locus.forward"]]=input$locusStrand=="+"
      locus_params[["flank.start"]]=as.integer(input$upstreamMinus)
      locus_params[["flank.end"]]=as.integer(input$downstreamPlus)
      locus_params[["Tstar"]]=if(input$locusIncludeTref) Tref else NULL
      locus_params[["D"]]=if(input$locusIncludeD) meth.data[ind, sample_subset] else NULL
      locus_params[["plot.genes"]]=TRUE

    }else{
      locus_params<-NULL
    }
    }
  }
  MeDeCom:::plotLMCs(results,
           type = type,
           K =  K,
           lambda= lambda ,
           cg_subset = cg_,
           lmc = as.integer(input$component),
           Tref = Tref,
           distance = input$mdsDist ,
           center = input$correlationCentered,
           n.most.var = NA,
           D = server_env$getMethData()[ind, server_env$getSampleSubset()] ,
           sample.characteristic = data.ch ,
           scatter.matching = scatter.matching,
           scatter.smooth = TRUE,
           scatter.cg.feature = NULL
           #locus.parameters = locus_params
           )
}

server_env$doProportionPlot<-function(){
  results<-server_env$dataset()

  #cg subset
  gr_list <-results@parameters$cg_subsets
  gr<-as.integer(input$cg_group_4)
  cg_ <- gr_list[gr]

  #lambdas
  ll <- as.integer(input$lambda_4)
  lambdas <- results@parameters$lambdas
  lambda <- lambdas[ll]

  Aref<-server_env$getTrueA()
  ind<-server_env$getCGsubset_4()
  trueT<-server_env$getTrueT()
  Tref<-trueT[ind,]

  if(input$propPlotType == "heatmap"){
    if(input$mdsDataCat_4!="none"){
      pheno.data<-server_env$getPhenoData()
      data.ch<-pheno.data[[input$mdsDataCat_4]]
    }else{
      data.ch<-NULL
    }
  }
  else{
    data.ch<-NULL
  }

  MeDeCom:::plotProportions(results,
                  type = input$propPlotType,
                  K = as.integer(input$K_4),
                  lambda = lambda,
                  cg_subset = cg_,
                  lmc = as.integer(input$component_4),
                  Aref = Aref ,
                  ref.profile = as.integer(input$profile),
                  assignment.method = "pearson",
                  sample.characteristic=data.ch,
                  heatmap.clusterCols = input$propClusterCols,
                  heatmap.clusterRows = input$propClusterRows)
}
}
