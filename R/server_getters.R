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

}
