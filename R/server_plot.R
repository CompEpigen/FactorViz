server_plot<-function(input, output, server_env){
server_env$doKselectionPlot<-function(){
  server_env$df()
  withProgress(
    message = 'Plotting...\n',
    detail = '...',
    value = 0.3,
    {
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
  MeDeCom:::plot.K.selection(results, statistic = statistic, Ks = as.numeric(Kvals),lambdas = as.numeric(lambda), cg_subset = as.integer(cg_), sample_subset = sample_subset,cg_subsets = gr_list, KvsRMSElambdaLegend = TRUE, normalizedCVE = input$normalizedCVE, addPlotTitle = TRUE)
  }
})
}

server_env$doLambdaPlot<-function(){
  withProgress(
    message = 'Plotting...\n',
    detail = '...',
    value = 0.3,
    {
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
  includeRMSE_T<-F
  includeMAE_A<-F
  includeDist2C_T<-F
  if (!is.null(input$includeRMSE_T)){
    includeRMSE_T<-input$includeRMSE_T
  }
  if (!is.null(input$includeMAE_A)){
    includeMAE_A<-input$includeMAE_A
  }
  if (!is.null(input$includeRMSE_T)){
    includeDist2C_T<-input$includeDist2C_T
  }
  MeDeCom:::plot.lambda.selection(results, cg_subset = as.integer(cg_), K = k, minLambda = minLam, maxLambda =maxLam, scale= scale ,includeRMSE_T= includeRMSE_T, includeMAE_A = includeMAE_A, includeDist2C_T = includeDist2C_T )
})
}

server_env$doComponentPlot<-function(){
  withProgress(
    message = 'Plotting...\n',
    detail = '...',
    value = 0.3,
    {
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
  if (!is.null(trueT)){
  Tref<-trueT[ind,]
  }else{
    Tref<-NULL
  }

  #types
  type <- input$componentPlotType
  if (type == "mds plot"){
    type <- "MDS"
  }
  #that
  That<-results@outputs[[as.character(gr)]]$T[[index,ll]]


  #sample.characteristic
  data.ch<-NULL
  if(type=="MDS"){
    if(!is.null(input$mdsDataCat) && input$mdsDataCat!="none"){
      pheno.data<-server_env$getPhenoData()
      data.ch<-pheno.data[[input$mdsDataCat]]
    }
  }
  #top.cgs never used ?
  top.cgs <- NA
    if(!is.null(input$cgVarSubset) && input$cgVarSubset){
      top.cgs<-input$topSDcpgs
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
  if(!is.null(input$useReferences) && (!input$useReferences)){
      Tref<-NULL
      D<-NULL
      data.ch<-NULL
  }else{
    D<- server_env$getMethData()[ind, server_env$getSampleSubset()]
  }
  min.similarity=0
  if(!is.null(input$minGraphSimilarity)){
    min.similarity=input$minGraphSimilarity
  }
  MeDeCom::plotLMCs(results,
           type = type,
           K =  K,
           lambda= lambda ,
           cg_subset = cg_,
           lmc = as.integer(input$component),
           Tref = Tref,
           distance = input$mdsDist ,
           center = input$correlationCentered_3,
           n.most.var = NA,
           D = D,
           sample.characteristic = data.ch ,
           scatter.matching = scatter.matching,
           scatter.smooth = TRUE,
           scatter.cg.feature = NULL,

           )
})
}

server_env$doProportionPlot<-function(){
  withProgress(
    message = 'Plotting...\n',
    detail = '...',
    value = 0.3,
    {
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

  if(input$propPlotType == "heatmap" || input$propPlotType == "correlations"){
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
  MeDeCom::plotProportions(results,
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
})
}

server_env$doComparisonPlot<-function(){
  withProgress(
    message = 'Plotting...\n',
    detail = '...',
    value = 0.3,
    {

  if(length(input$compareMatrices)>0){

    cg_subset<-system_env$getCGsubset()
    cg_subset_ref<-system_env$getCGsubsetRef()
    sample_subset<-system_env$getSampleSubset()
    sample_subset_ref<-system_env$getRefSampleSubset()

    cpg_intersect<-intersect(cg_subset, cg_subset_ref)

    if(length(cpg_intersect)<1){
      return(invisible(NULL))
    }


    cg_map<-match(cpg_intersect,cg_subset)
    cg_map_ref<-match(cpg_intersect, cg_subset_ref)

    #cg_map<-cg_subset

    mdd<-matrix(NA, nrow=length(cpg_intersect), ncol=0)

    results <-system_env$dataset()
    gr<-as.integer(input$cg_group_5)
    ll<-as.integer(input$lambda_5)
    K<-as.integer(input$K_5)
    #finds out the index of k in Ks
    Ks<-results@parameters$Ks
    index <- NULL
    for (i in 1:length(Ks)){
      if(Ks[i]==K){
        index <- as.numeric(i)
      }
    }

    ll_ref<-as.integer(input$lambda_ref)
    sds<-NULL

    if("That" %in% input$compareMatrices){
      That<-system_env$dataset()@outputs[[as.character(gr)]]$T[[index,ll]][cg_map,]

      colnames(That)<-paste0(paste0(input$analysisToken, "_LMC"), 1:ncol(That))
      if(input$correlationCentered){
        mdd<-cbind(mdd,sweep(That,1,rowMeans(That),"-"))
      }else{
        mdd<-cbind(mdd, That)
      }
      if(input$SDCompareMatrices=="That"){
        sds<-apply(That,1,sd)
      }
    }

    if("D" %in% input$compareMatrices){
      D<-system_env$getMethData()[cg_subset,sample_subset][cg_map,]
      sn<-system_env$getSampleNames()[sample_subset]
      if(is.null(sn)){
        colnames(D)<-paste(paste0(input$analysisToken, "_D"), 1:ncol(D), sep="_")
      }else{
        colnames(D)<-sn
      }
      if(input$correlationCentered){
        mdd<-cbind(mdd,sweep(D,1,rowMeans(D),"-"))
      }else{
        mdd<-cbind(mdd, D)
      }
      if(input$SDCompareMatrices=="D"){
        sds<-apply(D,1,sd)
      }
    }

    if("Tstar" %in% input$compareMatrices){
      Tref<-system_env$getTrueT()[cg_subset,][cg_map,]
      if(is.null(colnames(Tref))){
        colnames(Tref)<-paste(paste0(input$analysisToken, "_Tstar"), 1:ncol(Tref), sep="_")
      }
      if(input$correlationCentered){
        mdd<-cbind(mdd,sweep(Tref,1,rowMeans(Tref),"-"))
      }else{
        mdd<-cbind(mdd, Tref)
      }
      if(input$SDCompareMatrices=="Tstar"){
        sds<-apply(Tref,1,sd)
      }
    }

    samps<-1:ncol(mdd)

    if("refThat" %in% input$compareMatrices){
      print(input$K_ref)
      That<-server_env$dataset_ref()@outputs[[as.character(gr)]]$T[[input$K_ref,ll_ref]][cg_map_ref,]

      colnames(That)<-paste0(paste0(input$refAnalysisToken, "_LMC"), 1:ncol(That))
      if(input$correlationCentered){
        mdd<-cbind(mdd,sweep(That,1,rowMeans(That),"-"))
      }else{
        mdd<-cbind(mdd,That)
      }

    }

    if("refD" %in% input$compareMatrices){
      D<-server_env$getRefMethData()[cg_subset_ref,sample_subset_ref][cg_map_ref,]
      sn<-server_env$getRefSampleNames()[sample_subset_ref]
      if(is.null(sn)){
        colnames(D)<-paste(paste0(input$refAnalysisToken,"D"), 1:ncol(D), sep="_")
      }else{
        colnames(D)<-paste(paste0(input$refAnalysisToken,"_D_"), sn, sep="_")
      }
      if(input$correlationCentered){
        mdd<-cbind(mdd,sweep(D,1,rowMeans(D),"-"))
      }else{
        mdd<-cbind(mdd,D)
      }
    }

    if("refTstar" %in% input$compareMatrices){
      Tref<-server_env$getRefTrueT()[cg_subset_ref,][cg_map_ref,]
      if(is.null(colnames(Tref))){
        colnames(Tref)<-paste(paste0(input$refAnalysisToken,"Tstar"), 1:ncol(Tref), sep="_")
      }else{
        colnames(Tref)<-paste(paste0(input$refAnalysisToken,"_Tstar_"), colnames(Tref), sep="_")

      }
      if(input$correlationCentered){
        mdd<-cbind(mdd,sweep(Tref,1,rowMeans(Tref),"-"))
      }else{
        mdd<-cbind(mdd,Tref)
      }

    }

    if(input$topSDcpgsCompare<nrow(mdd)){
      if(is.null(sds)){
        sds<-apply(mdd,1,sd)
      }
      mdd<-mdd[order(sds, decreasing=TRUE)[1:input$topSDcpgsCompare], ]
    }

    if(ncol(mdd)>length(samps)){
      ref_samps<-(samps[length(samps)]+1):ncol(mdd)
    }else{
      ref_samps<-NULL
    }

    if(input$comparativePlotType=="dendrogram"){

      if(input$mdsDist=="euclidean"){
        d <- dist(t(mdd))
      }else{
        d<-as.dist(1-cor(mdd))
      }
      labelColors <- c("red","blue")
      colLab <- function(n) {
        if (is.leaf(n)) {
          a <- attributes(n)
          labCol <- labelColors[as.integer(grepl(input$refAnalysisToken, a$label))+1]
          attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
        }
        n
      }

      hcl_obj<-hclust(d, method="average")
      dendr<-as.dendrogram(hcl_obj)
      dendr<-dendrapply(dendr, colLab)

      if(!is.null(runPart)){
        plot(dendr)
        return(hcl_obj)
      }else{
        plot(dendr)
      }

    }else if(input$comparativePlotType=="heatmap"){

      max_rows<-20000

      heatmap.2(mdd[1:min(max_rows, nrow(mdd)),],
                trace="none", scale="none", col=zero2oneCols, margins=c(10,5), labRow=FALSE)

    }else if(input$comparativePlotType=="correlation heatmap"){

      MeDeCom:::components.heatmap(mdd[,samps,drop=FALSE], if(!is.null(ref_samps)) mdd[,ref_samps,drop=FALSE] else NULL, margins=c(5,7), cexRow=1, top.sd.cgs=NA, centered=input$correlationCentered)

    }else{

    }
  }
})
}



server_env$doDiffCGPlot<-function(){
  #values$change
  withProgress(
    message = 'Plotting...\n',
    detail = '...',
    value = 0.3,
    {
  results<-server_env$dataset()
  gr<-as.integer(input$cg_group_5)
  ll<-as.integer(input$lambda_5)
  K<-input$K_5
  #finds out the index of k in Ks
  Ks<-results@parameters$Ks
  index <- NULL
  for (i in 1:length(Ks)){
    if(Ks[i]==K){
      index <- as.numeric(i)
    }
  }

  cmp_group1<-as.integer(input$componentGroup1)
  cmp_group2<-as.integer(input$componentGroup2)

  That<-results@outputs[[gr]]$T[[index,ll]]

  if(length(cmp_group1)>0 && length(cmp_group2)>0){

    meth_diff<-rowMeans(That[,cmp_group1,drop=FALSE])-rowMeans(That[,cmp_group2,drop=FALSE])

    xl<-min(1.0, max(abs(meth_diff))+0.1)

    hist(meth_diff, breaks=200, xlim=c(-xl,xl), main="", xlab="(mean) methylation difference")
    abline(v=input$dmr_threshold)
    abline(v=-input$dmr_threshold)
    if(input$addPlotTitle){
      title(sprintf("LMC(s) %s vs LMC(s) %s",
                    paste(input$componentGroup1, collapse=", "),
                    paste(input$componentGroup2, collapse=", ")
      ))
    }
  }
})
}

server_env$doPhenotypeModelPlot<-function(){
  withProgress(
    message = 'Plotting...\n',
    detail = '...',
    value = 0.3,
    {
  results<-server_env$dataset()
  lls<-sort(results@parameters$lambdas)
  Kvals<-results@parameters$Ks
  Ks<-results@parameters$Ks
  index <- NULL
  for (i in 1:length(Ks)){
    if(Ks[i]==Kvals){
      index <- as.numeric(i)
    }
  }

  pheno_data<-server_env$getPhenoData()[server_env$getSampleSubset(),,drop=FALSE]

  pval_matrix<-matrix(0, ncol=length(Kvals),nrow=length(lls))

  blue.cols<-colorRampPalette(c("white", "blue"))

  target_var<-input$phenoTarget
  adj_vars<-input$phenoAdjust

  for(k in Kvals){
    for(lambda in sort(lls)){
      Ahat<-results@outputs[[input$cg_group_5]]$A[[as.integer(as.character(index)),match(lambda, lls)]]

      fit<-fitPhenotypeModel(Ahat, pheno_data, target_var, adj_vars, input$zeroLevel, input$discardLMC)

      sf<-summary(fit)
      global.p<-pf(sf$fstatistic[1], sf$fstatistic[2], sf$fstatistic[3], lower.tail=FALSE)
      min.cmp.p<-min(sf$coefficients[-1,4])

      if(input$modelPval=="overall"){
        pval_matrix[which(lls==lambda),which(Kvals==k)]<-global.p
      }else if(input$modelPval=="minimal"){
        pval_matrix[which(lls==lambda),which(Kvals==k)]<-min.cmp.p
      }
    }}
  N_COL_BINS<-10

  heatmap.2(-log10(pval_matrix),
            cellnote=matrix(as.character(round(-log10(pval_matrix),3)),ncol=ncol(pval_matrix)),
            col=blue.cols(N_COL_BINS-1),
            labCol=as.character(Kvals), labRow=sprintf("%g", lls),
            scale="none", trace="none", Colv=NA, Rowv=NA,
            key.xlab="-log10(p)", key.title="")
})
}

##################################################################################################
################################ Meta Analysis Plots #############################################
##################################################################################################
server_env$doMetaPlot<-function(){
  withProgress(
    message = 'Plotting...\n',
    detail = '...',
    value = 0.3,
    {
  server_env$getLOLAEnrichmenttable()
  if(!is.null(input$lmc_lola)){
    if(!is.na(server_env$getLOLAEnrichmenttable()[[input$lmc_lola]])){
      MeDeCom:::do.lola.plot(server_env$getLOLAEnrichmenttable()[[input$lmc_lola]],lola.db,pvalCut=0.01)
    }
}
})
}
server_env$doTraitAssociation<-function(){
  withProgress(message="Associating Traits...\n", detail='...', value=0.3,{
    results<-server_env$dataset()
    gr_list <- results@parameters$cg_subsets
    gr<-as.integer(input$cg_group_5)
    cg_ <- gr_list[gr]
    ll<-as.integer(input$lambda_5)
    lambdas <- results@parameters$lambdas
    lambda <- lambdas[ll]
    K<-input$K_5
    out<-MeDeCom::run.trait.association.single(results, server_env$getPhenoData(), cg_subset=cg_, K=K, lambda=lambda, test.fun=t.test)
    if(!is.null(input$tatstat)){
      return(out[[input$tatstat]])
    }else{
      return(br())
    }

  })
}
}
