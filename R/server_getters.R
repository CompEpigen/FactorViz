# This file contains the code related to the getters in server

##############################################
########### Getter functions##################
##############################################

server_getters<-function(input, output, server_env){
  server_env$getAnalysisName<-function(){
    an<-server_env$dataset()@dataset_info$analysis_name
    if(is.null(an)){
      an<-"Unnamed analysis"
    }
    an
  }
  
  server_env$getInputDataset<-reactive(quote({
    input_object
    }), quoted=TRUE)
  
  server_env$getMethData <-reactive(quote({
    server_env$getInputDataset()$meth.data
    }),quoted=TRUE)

  server_env$getTrueT <- reactive(quote({
    true_T_matrix
    }),quoted=TRUE)

  server_env$getRuns<-reactive(quote({
    list()
  }),quoted=TRUE)

  server_env$getCGsubset_3<-reactive(quote({
    server_env$dataset()@parameters$GROUP_LISTS[[as.integer(input$cg_group_3)]]
  }),quoted=TRUE)

  server_env$getCGsubset_4<-reactive(quote({
    server_env$dataset()@parameters$GROUP_LISTS[[as.integer(input$cg_group_4)]]
  }),quoted=TRUE)
  
  server_env$getCGsubset_5<-reactive(quote({
    server_env$dataset()@parameters$GROUP_LISTS[[as.integer(input$cg_group_5)]]
  }),quoted=TRUE)

  server_env$getPhenoData<-reactive(quote({
    server_env$getInputDataset()$pheno.data
  }),quoted=TRUE)

  server_env$getCGcategories<-reactive(quote({
    prepared_annotation[["cat_list"]]
  }),quoted=TRUE)

  server_env$getCGquantFeatureSettings<-reactive(quote({
    server_env$prepared_annotation[["settings"]]
  }),quoted=TRUE)

  server_env$getGeneSets<-reactive(quote({
    server_env$gene_sets_object
  }),quoted=TRUE)

  server_env$getTTandTrefSDranking<-reactive(quote({
    results<-server_env$dataset()
    gr<-as.integer(input$cg_group_3)
    ll<-as.integer(input$lambda_3)
    cmp<-as.integer(input$component)
    ind<-server_env$getCGsubset()
    trueT<-server_env$getTrueT()
    Tref<-trueT[ind,]
    sds<-apply(cbind(Tref, results@outputs[[input$K_3]]$T[[gr,ll]]), 1, sd)
    order(sds, decreasing = TRUE)
  }), quoted=TRUE)

  server_env$getSampleSubset<-reactive(quote({
    server_env$dataset()@parameters$sample_subset
  }), quoted=TRUE)

  server_env$getCGAnnot<-reactive(quote({
    cg_annot_object
  }),quoted=TRUE)

  server_env$getGeneAnnot<-reactive(quote({
    gene_annot_object
  }),quoted=TRUE)

  server_env$getTrueA <- reactive(quote({
    true_A_matrix
  }),quoted=TRUE)
  
  server_env$dataset_ref <- reactive(quote({
      results<-medecom_ref_object
    results
  }),quoted=TRUE)



server_env$getCGsubsetRef<-reactive(quote({
  server_env$dataset_ref()@parameters$cg_subset_lists[[as.integer(input$cg_group_ref)]]
}),quoted=TRUE)

server_env$getGREATresults<-reactive(quote({
  
  input$GREATsubmitQuery			
  if(input$diffTableType=="hypomethylated"){
    diff<-getDiffCGs()[[1]]
  }else{
    diff<-getDiffCGs()[[2]]
  }
  
  if(nrow(diff)>0){
    
    bed<-diff[,c(3:5)]
    rownames(bed)<-diff[,c(1)]
    bg<-server_env$getCGAnnot()[server$getCGsubset(),c(1:3)]
    isolate({						
      #	incProgress(1/2, detail = "Submitted job")
      withProgress(message="Performing query...", value=0, {
        job <- submitGreatJob(gr=bed, 
                              species="hg19", 
                              bgChoice = "file", bg=bg,
                              rule = input$GREATrule, 
                              adv_oneDistance = input$GREATmaxDistance,
                              adv_twoDistance = input$GREATmaxDistance,
                              adv_upstream = input$GREATmaxDistance/2,
                              adv_downstream = input$GREATmaxDistance/10,
                              adv_span = input$GREATmaxDistance*10,
                              request_interval = 30)
        incProgress(1, detail = "Done!")
      })
    })
    #	incProgress(1/2, detail = "Fetching results")
    job
    #})
    
  }else{
    NULL
  }
  
}), quoted=TRUE)

server_env$getDiffCGs<-reactive(quote({
  #values$change
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
  
  That<-server_env$dataset()@outputs[[gr]]$T[[index,ll]]
  
  if(length(input$componentGroup2)>0){
    cmp_group2<-as.integer(input$componentGroup2)
    
    meth_diff<-rowMeans(That[,cmp_group1,drop=FALSE])-rowMeans(That[,cmp_group2,drop=FALSE])
    
    hypo_cgs<-which(meth_diff <= (-1)*input$dmr_threshold)
    hyper_cgs<-which(meth_diff>=input$dmr_threshold)
    
  }else{
    meth_diff<-rowMeans(That[,cmp_group1,drop=FALSE])
    hypo_cgs<-which(meth_diff < input$dmr_threshold)
    hyper_cgs<-which(meth_diff > input$dmr_threshold)
  }
  
  
  ind_hypo<-server_env$getCGsubset_5()[hypo_cgs]
  ind_hyper<-server_env$getCGsubset_5()[hyper_cgs]
  
  ann_hypo<-server_env$getCGAnnot()[ind_hypo,,drop=FALSE]
  ann_hyper<-server_env$getCGAnnot()[ind_hyper,,drop=FALSE]
  
  ann_hypo<-cbind(data.frame(ID=rownames(ann_hypo), Diff=meth_diff[hypo_cgs]), ann_hypo)
  ann_hyper<-cbind(data.frame(ID=rownames(ann_hyper), Diff=meth_diff[hyper_cgs]), ann_hyper)
  
  list(ann_hypo=ann_hypo, ann_hyper=ann_hyper)
  
}), quoted=TRUE)

server_env$getRefSampleSubset<-reactive(quote({
  server_env$dataset_ref()@parameters$sample_subset
}), quoted=TRUE)
}