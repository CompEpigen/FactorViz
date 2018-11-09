#This file contains the output objects of the server

#############################################
########### main panel outputs ##############
#############################################

##########################################  Home tab main panel

output$files <- renderPrint(
  list.files(path())
  )

output$AnalysisRunDescriptionHeader<-renderUI({
  df()
  if (MEDSET_FLAG){
    wellPanel(
      #strong("Analysis Run:"),
      h4(getAnalysisName())
    )
  }else{
    wellPanel(
      strong("Please load a dataset")
    )
  }
})

output$AnalysisRunParameterTable<-renderTable({
  df()
  if(MEDSET_FLAG){
    withProgress(message = 'Loading datasets in progress\n',
                 detail = 'This may take a while...', value = 0, style = getShinyOption('old'),
                 {
    output<-list()
    print("Starting table")
    results<-dataset()
    print("...............................MeDeComSet...........................")
    print("...............................MeDeComSet_ende...........................")
    
    for(name in names(ANALYSIS_META_INFO[["analysis_params"]])){
      incProgress(1/length(names(ANALYSIS_META_INFO[["analysis_params"]])))
      display_name<-ANALYSIS_META_INFO[["analysis_params"]][name]
      if(name %in% names(results@parameters)){
        val<-results@parameters[[name]]
        if(name %in% c("lambdas")){
          val<-sort(val)						
        }
        
        if(is.integer(val)){
          val<-as.character(val)
        }else{
          if(name %in% names(ANALYSIS_META_INFO[["param_extensions"]])){
            val_ext<-ANALYSIS_META_INFO[["param_extensions"]][[name]][val]
            if(!is.na(val_ext)){
              val<-val_ext
            }
          }
        }
      }else if(name %in% "NO_OF_SAMPLES"){
        val<-as.character(ncol(getMethData()))
      }else if(name %in% "REFERENCE_PROFILES"){
        val<-paste(colnames(getTrueT()), collapse=", ")
      }else{
        val=""
      }
      if(length(val)==0){
        val=""
      }
      if(is.recursive(val) || val!=""){
        output[[name]]<-c(display_name, paste(val, collapse=", "))
      }
    }
    table<-do.call("rbind", output)
    rownames(table)<-NULL
    colnames(table)<-c("Parameter", "Value")
    table
                 })
  }else{
    
  }
}, width=1000, include.rownames=FALSE)

##################################### K selection main panel
output$RMSEvsKplot<-renderPlot({
  df()
  if(MEDSET_FLAG){
  doKselectionPlot()
  }
})

######################## Lambda selection main panel
output$performancePanel<-renderUI({
  df()
  if(!is.null(input$performanceMode)&&   !is.null(input$lambdaMin) && !is.null(input$lambdaMax)){
  if(input$performanceMode=="lineplots"){
    list(plotOutput('lineplot', 
                    height = "800px",
                    width = "600px"),
         br(),
         downloadLink("lineplotPDF", "PDF"))
  }else if(input$performanceMode=="table"){
    tableOutput('performanceTable')
  }
  }
})

output$lineplot <- renderPlot({
  df()
  doLambdaPlot()
})

output$performanceTable<-renderTable({
  df()
  results<-dataset()
  gr<-as.integer(input$cg_group_2)
  elts<-PERFORMANCE_MEASURES
  output<-list()
  test_Ks<-input$K_2
  Ks<-results@parameters$Ks
  index <- match(test_Ks, Ks)
  for(elt in 1:length(elts)){
    print(elts[elt])
    min_lls<-integer(length(test_Ks))
    min_vals<-double(length(test_Ks))
    if (all(is.na(results@outputs[[gr]][[elts[elt]]]))){
      next
    }
    for(kk in 1:length(test_Ks)){
      #use the index instead of test_Ks
      min_lls[kk]<-which.min(results@outputs[[gr]][[elts[elt]]][index,,drop=F])
      min_vals[kk]<-results@outputs[[gr]][[elts[elt]]][index,min_lls[kk],drop=F]
    }
    min_kk<-which.min(min_vals)
    output[[elt]]<-c(names(elts)[elt], sprintf("%f",min(min_vals[min_kk])), sprintf("%f", results@parameters$lambdas[min_lls[min_kk]]))
    if(length(test_Ks)>1){
      output[[elt]]<-c(output[[elt]], sprintf("%d",test_Ks[kk]))
    }
  }
  output<-do.call("rbind", output)
  rownames(output)<-NULL
  cn<-c("Statistic", "Minimal value", "Lambda")
  if(length(test_Ks)>1){
    cn<-c(cn, "K")
  }
  colnames(output)<-cn
  output
}, include.rownames=FALSE)

######################## LMC main panel
output$componentsPanel<-renderUI({
  df()
  K<-input$K_3
  if(!is.null(input$componentPlotType)){
  if(input$componentPlotType=="mds plot" || input$componentPlotType=="dendrogram"){
    h="500px"
    w="500px"
  }else if(input$componentPlotType=="heatmap"){
    h="500px"
    w=sprintf("%dpx", max(500, 50*as.integer(K)))
  }else if(input$componentPlotType=="scatterplot matching"){
    h0=300
    w0=300
    ncol=min(3,as.integer(K))
    nrow=(as.integer(K) %/% min(3,as.integer(K))) + as.integer(as.integer(K) %% min(3,as.integer(K))>0)
    h=sprintf("%dpx", h0*nrow)
    w=sprintf("%dpx", h0*ncol)
  }else if(input$componentPlotType=="locus plot"){
    h=500
    w=1000
  }else{
    print("Hello")
    h0=300
    w0=300
    trueT<-getTrueT()
    ncol<-min(3, ncol(trueT))
    nrow<-ncol(trueT) %/% ncol + as.integer(ncol(trueT) %% ncol>0)
    h=sprintf("%dpx", h0*nrow)
    w=sprintf("%dpx", h0*ncol)
  }
  list(plotOutput('componentPlot',
                  height = h,
                  width =  w), br(),
       downloadLink("componentPlotPDF", "PDF"))
  }
  
})

output$componentPlot<-renderPlot({
  df()
  doComponentPlot()
})	

######################## Proportion Main Panel
output$proportionplot<-renderPlot({
  df()
  if(!is.null(input$propPlotType)){
  doProportionPlot()
  }
})

#############################################
############# Sidebar outputs ###############
#############################################

########################################### Sidebar outputs for K selection tab

output$groupSelector<-renderUI({
  df()
  if(MEDSET_FLAG){
  results<-dataset()
  gr_lists<-results@parameters$cg_subsets
  GROUPS<-1:length(gr_lists)
  if(is.null(names(gr_lists))){
    names(GROUPS)<-sapply(gr_lists, paste, collapse="_")
  }else{
    names(GROUPS)<-names(gr_lists)
  }
  selectInput('cg_group', 'Technical CpG subset:', GROUPS, selectize=TRUE)
  }
})

output$minKselector<-renderUI({
  df()
  if(MEDSET_FLAG){
    selectInput('minK', 'Minimum k:', dataset()@parameters$Ks)
  }
})

output$maxKselector<-renderUI({
  df()
  if(MEDSET_FLAG){
    selectInput('maxK', 'Maximum k:', dataset()@parameters$Ks, selected=dataset()@parameters$Ks[length(dataset()@parameters$Ks)])
  }
})

output$lambdaSelector<-renderUI({
  df()
  if(MEDSET_FLAG){
    LAMBDAS<-dataset()@parameters$lambdas
    LAMBDA.IDS<-1:length(dataset()@parameters$lambdas)
    names(LAMBDA.IDS)<-as.character(LAMBDAS)
    selectInput('lambda', 'Lambda value', LAMBDA.IDS)
  }
  
})

output$KvsStat<-renderUI({
  df()
  p_measure<-c("Objective"="Fval", "CV error"="cve")
  if(METH_DATA_FLAG){
    p_measure<-c(p_measure, "RMSE"="rmse", "RMSE, T"="rmseT",  "MDC, T"="dist2C", "MAE, A"="maeA")
  }
  selectInput("KvsStat", "Statistic to plot:",
              p_measure,
              selected=2)
})


######################################## Sidebar outputs for lambda selection tab

output$groupSelector_2<-renderUI({
  df()
  if(MEDSET_FLAG){
    results<-dataset()
    gr_lists<-results@parameters$cg_subsets
    GROUPS<-1:length(gr_lists)
    if(is.null(names(gr_lists))){
      names(GROUPS)<-sapply(gr_lists, paste, collapse="_")
    }else{
      names(GROUPS)<-names(gr_lists)
    }
    selectInput('cg_group_2', 'Technical CpG subset:', GROUPS, selectize=TRUE)
  }
})

output$Kselector_2<-renderUI({
  df()
  #Ks<-dataset()[["Ks"]]
  Ks<-dataset()@parameters$Ks
  selectInput('K_2', 'Number of LMCs (k)', Ks, selectize=TRUE)
})

output$minLambdaSelector<-renderUI({
  df()
  lambda_list<-sort(dataset()@parameters$lambdas)
  selectInput('lambdaMin', 'Minimum lambda:', lambda_list, selected=lambda_list[which.min(lambda_list)])
})

output$maxLambdaSelector<-renderUI({
  df()
  lambda_list<-sort(dataset()@parameters$lambdas)
  selectInput('lambdaMax', 'Maximum lambda:', lambda_list, selected=lambda_list[which.max(lambda_list)])
})

output$performanceModeSelector<-renderUI({
  df()
    selectInput('performanceMode', 'Show results as:', c("lineplots","table"), selectize=TRUE)
  })

#################################################### Sidebar output LMCs panel

output$groupSelector_3<-renderUI({
  df()
  if(MEDSET_FLAG){
    results<-dataset()
    gr_lists<-results@parameters$cg_subsets
    GROUPS<-1:length(gr_lists)
    if(is.null(names(gr_lists))){
      names(GROUPS)<-sapply(gr_lists, paste, collapse="_")
    }else{
      names(GROUPS)<-names(gr_lists)
    }
    selectInput('cg_group_3', 'Technical CpG subset:', GROUPS, selectize=TRUE)
  }
})

output$Kselector_3<-renderUI({
  df()
  #Ks<-dataset()[["Ks"]]
  Ks<-dataset()@parameters$Ks
  selectInput('K_3', 'Number of LMCs (k)', Ks, selectize=TRUE)
})

output$lambdaSelector_3<-renderUI({
  df()
  if(MEDSET_FLAG){
    LAMBDAS<-dataset()@parameters$lambdas
    LAMBDA.IDS<-1:length(dataset()@parameters$lambdas)
    names(LAMBDA.IDS)<-as.character(LAMBDAS)
    selectInput('lambda_3', 'Lambda value', LAMBDA.IDS)
  }
  
})

output$componentPlotT<-renderUI({
  df()
  p_type<-c("dendrogram", "extremality", "heatmap", "mds plot", "similarity graph", "scatterplot all","scatterplot matching","scatterplot avg matching")
  if(METH_DATA_FLAG){
    p_type<-c(p_type, "distance to center")
    
  }
  selectInput('componentPlotType', 'Plot type', 
              p_type, 
              selected=1)
  
})

output$componentSelector<-renderUI({
  df()
  comps<-c(1:input$K_3, NA)
  names(comps)<-c(as.character(1:input$K_3), "sum best matching")
  selectInput('component', 'LMC:', comps, selectize=TRUE, multiple=TRUE, selected=1)#isolate({if("component" %in% names(input)) as.character(input$component) else 1}))
})

output$topSDcgsSelector<-renderUI({
  df()
  gr<-as.integer(input$cg_group_3)
  ind<-getCGsubset()
  sliderInput('topSDcpgs', 'Select top SD cgs', min=1, max=length(ind),
              value=length(ind), step=500, round=0)
})

output$sampleColorSelector<-renderUI({
  df()
  pd<-getPhenoData()
  if(!is.null(pd)){
    siteannot<-colnames(pd)
  }else{
    siteannot<-character()
  }
  selectInput('mdsDataCat', 'Color samples by:', c("none", siteannot), selectize=TRUE)
})

output$pointColorSelector<-renderUI({
  df()
  cats<-names(getCGcategories())
  feats<-names(getCGquantFeatureSettings())
  selectInput('pointCategory', 'Color data points by:', c("none", cats, feats), selectize=TRUE)
})

output$pointFilter<-renderUI({
  df()
  if("pointCategory" %in% names(input) && input$pointCategory!="none"){
    if(input$pointCategory %in% names(getCGcategories())){
      list(
        checkboxInput('CGsubsetToCategory', "limit to", value=FALSE)
      )
    }else if(input$pointCategory %in% names(getCGquantFeatureSettings())){
      features<-getCGquantFeatureSettings()
      list(
        sliderInput('quantFilterMin', "Minimal value:", min=features[[input$pointCategory]]$min, max=features[[input$pointCategory]]$max, value=features[[input$pointCategory]]$min),
        sliderInput('quantFilterMax', "Maximal value:", min=features[[input$pointCategory]]$min, max=features[[input$pointCategory]]$max, value=features[[input$pointCategory]]$max)
      )
    }
  }
})

output$CGcategorySubsetSelector<-renderUI({
  df()
  if("CGsubsetToCategory" %in% names(input) && input$CGsubsetToCategory && input$pointCategory!="none"){
    cats<-getCGcategories()
    features<-1:length(cats[[input$pointCategory]])
    names(features)<-cats[[input$pointCategory]]
    selectInput('pointSubset', 'Show only:', features, selectize=TRUE, multiple=TRUE, selected=1)
  }
})

output$geneSetSelector<-renderUI({
  df()
  gene.sets<-getGeneSets()				
  selectInput('geneSet', 'Gene set:', names(gene.sets), selectize=TRUE)
})

output$locusSelector<-renderUI({
  df()
  gene.sets<-getGeneSets()
  selectInput('locus', 'Gene:', gene.sets[[input$geneSet]], selectize=TRUE)
})


#################################################### Sidebar output Proportions panel

output$groupSelector_4<-renderUI({
  df()
  if(MEDSET_FLAG){
    results<-dataset()
    gr_lists<-results@parameters$cg_subsets
    GROUPS<-1:length(gr_lists)
    if(is.null(names(gr_lists))){
      names(GROUPS)<-sapply(gr_lists, paste, collapse="_")
    }else{
      names(GROUPS)<-names(gr_lists)
    }
    selectInput('cg_group_4', 'Technical CpG subset:', GROUPS, selectize=TRUE)
  }
})

output$Kselector_4<-renderUI({
  df()
  #Ks<-dataset()[["Ks"]]
  Ks<-dataset()@parameters$Ks
  selectInput('K_4', 'Number of LMCs (k)', Ks, selectize=TRUE)
})

output$lambdaSelector_4<-renderUI({
  df()
  if(MEDSET_FLAG){
    LAMBDAS<-dataset()@parameters$lambdas
    LAMBDA.IDS<-1:length(dataset()@parameters$lambdas)
    names(LAMBDA.IDS)<-as.character(LAMBDAS)
    selectInput('lambda_4', 'Lambda value', LAMBDA.IDS)
  }
  
})

output$propPlotT<-renderUI({
  df()
  p_measure<-c("heatmap", "barplot", "lineplot", "scatterplot", "stratification plot", "correlations")
  selectInput('propPlotType', 'Plot type', 
              p_measure, 
              selected=1)  
})

output$propMatrixSelector<-renderUI({
  df()
  if(!is.null(input$propPlotType)){
  if(input$propPlotType=="scatterplot"){
    prop_mats<-c("regression")
    labl<-"Reference proportions:"
  }else{
    prop_mats<-c("MeDeCom", "regression")
    labl<-"Proportions:"
  }
  if("analysisrun_ref" %in% names(input)){
    prop_mats<-c(prop_mats,"regression ref cmp")
  }
  selectInput('propMatrix', labl, prop_mats, selectize=TRUE)
  }
  
})

output$componentSelectorRef<-renderUI({
  df()
  comps<-c(1:input$K_ref)
  selectInput('component_ref', 'LMC (ref analysis):', comps, selectize=TRUE, multiple=TRUE, selected=isolate({
    if("component_ref" %in% names(input))as.character(input$component_ref) else 1
  }))
})

output$componentSelector_4<-renderUI({
  df()
  comps<-c(1:input$K_4, NA)
  names(comps)<-c(as.character(1:input$K_4), "sum best matching")
  selectInput('component_4', 'LMC:', comps, selectize=TRUE, multiple=TRUE, selected=1)#isolate({if("component" %in% names(input)) as.character(input$component) else 1}))
})

output$refProfileSelector<-renderUI({
  df()
  rprofiles<-c(NA)
  rprofile_names<-c("best_matching")
  if(!is.null(getTrueT())){ 
    rprofiles<-c(rprofiles, 1:ncol(getTrueT()))
    rprofile_names_add<-colnames(getTrueT())
    if(is.null(rprofile_names_add)){
      rprofile_names_add<-as.character(1:ncol(getTrueT()))
    }
    rprofile_names<-c(rprofile_names, rprofile_names)
  }
  names(rprofiles)<-rprofile_names
  selectInput('profile', 'Reference Profile:', rprofiles, selectize=TRUE, multiple=TRUE)
  
})

output$sampleColorSelector_4<-renderUI({
  df()
  pd<-getPhenoData()
  if(!is.null(pd)){
    siteannot<-colnames(pd)
  }else{
    siteannot<-character()
  }
  selectInput('mdsDataCat_4', 'Color samples by:', c("none", siteannot), selectize=TRUE)
})
