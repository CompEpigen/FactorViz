# This file contains the code related to the getters in server

##############################################
########### Getter functions##################
##############################################


getAnalysisName<-function(){
  an<-dataset()@dataset_info$analysis_name
  if(is.null(an)){
    an<-"Unnamed analysis"
  }
  an
}

getInputDataset<-reactive(quote({
      input_object
}), quoted=TRUE)

getMethData <-reactive(quote({
  getInputDataset()$meth.data
}),quoted=TRUE)


getTrueT <- reactive(quote({
    true_T_matrix
}),quoted=TRUE)

getRuns<-reactive(quote({
    list()
}),quoted=TRUE)


getCGsubset<-reactive(quote({
  dataset()@parameters$GROUP_LISTS[[as.integer(input$cg_group_3)]]
}),quoted=TRUE)

getPhenoData<-reactive(quote({
  getInputDataset()$pheno.data
}),quoted=TRUE)


getCGcategories<-reactive(quote({
    prepared_annotation[["cat_list"]]
}),quoted=TRUE)

getCGquantFeatureSettings<-reactive(quote({
  prepared_annotation[["settings"]]
}),quoted=TRUE)


getGeneSets<-reactive(quote({
    gene_sets_object
}),quoted=TRUE)

getTTandTrefSDranking<-reactive(quote({
  results<-dataset()
  gr<-as.integer(input$cg_group_3)
  ll<-as.integer(input$lambda_3)
  cmp<-as.integer(input$component)
  ind<-getCGsubset()
  trueT<-getTrueT()
  Tref<-trueT[ind,]
  sds<-apply(cbind(Tref, results@outputs[[input$K_3]]$T[[gr,ll]]), 1, sd)
  order(sds, decreasing = TRUE)
}), quoted=TRUE)

getSampleSubset<-reactive(quote({
  dataset()@parameters$sample_subset
}), quoted=TRUE)

getCGAnnot<-reactive(quote({
  cg_annot_object
}),quoted=TRUE)

getGeneAnnot<-reactive(quote({
  gene_annot_object
}),quoted=TRUE)

getTrueA <- reactive(quote({
    true_A_matrix
}),quoted=TRUE)