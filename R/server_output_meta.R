server_output_meta <- function(input, output, server_env) {
  output$groupSelector_5 <- renderUI({
    if (MEDSET_FLAG) {
      results <- server_env$dataset()
      gr_lists <- results@parameters$cg_subsets
      GROUPS <- 1:length(gr_lists)
      if (is.null(names(gr_lists))) {
        names(GROUPS) <- sapply(gr_lists, paste, collapse = "_")
      } else{
        names(GROUPS) <- names(gr_lists)
      }
      selectInput('cg_group_5', 'Technical CpG subset:', GROUPS,selected=server_env$Selected$CG)
    }
  })

  output$Kselector_5 <- renderUI({
    Ks <- server_env$dataset()@parameters$Ks
    selectInput('K_5', 'Number of LMCs (k)', Ks,selected=server_env$Selected$K)
  })

  output$lambdaSelector_5 <- renderUI({
    if (MEDSET_FLAG) {
      LAMBDAS <- server_env$dataset()@parameters$lambdas
      LAMBDA.IDS <- 1:length(server_env$dataset()@parameters$lambdas)
      names(LAMBDA.IDS) <- as.character(LAMBDAS)
      selectInput('lambda_5', 'Lambda value', LAMBDA.IDS, selected=server_env$Selected$LAMBDA)
    }
  })
  output$compareRunSelector <- renderUI({
    runs <- names(server_env$getRuns())
    names(runs) <- paste(seq_len(length(runs)), runs, sep = ". ")
    selectInput(
      'analysisrun_ref',
      '',
      runs,
      selectize = TRUE,
      width = "750px",
      selected = 1
    )
  })

  output$Kselector_ref <- renderUI({
    Ks <- server_env$dataset_ref()@parameters$Ks
    selectInput('K_ref', 'Number of LMCs (reference)', Ks, selectize = TRUE)
  })

  output$lambdaSelector_ref <- renderUI({
    LAMBDAS <- server_env$dataset_ref()@parameters$lambdas
    LAMBDA.IDS <- 1:length(dataset_ref()@parameters$lambdas)
    names(LAMBDA.IDS) <- as.character(LAMBDAS)
    selectInput('lambda_ref', 'Lambda value (reference)', LAMBDA.IDS)
  })

  output$topSDcgsSelectorCompare <- renderUI({
    gr <- as.integer(input$cg_group_5)
    ind <- server_env$getCGsubset()

    ind_ref <- server_env$getCGsubsetRef()
    ind_common <- intersect(ind, ind_ref)

    sliderInput(
      'topSDcpgsCompare',
      'Select top SD cgs',
      min = 100,
      max = length(ind),
      value = length(ind),
      step = 100,
      round = 0
    )
  })
  output$analysisTokensInput <- renderUI({
    dn <- "Target"
    dn_ref <- "Reference"
    list(
      textInput("analysisToken", "Analysis token:", dn),
      textInput("refAnalysisToken", "Reference analysis token:", dn_ref)
    )
  })

  output$dmCGComponentSelector <- renderUI({
    if(!is.null(input$K_5)){
    cmp_choices = as.list(as.character(1:as.integer(input$K_5)))
    names(cmp_choices) = c(as.character(1:as.integer(input$K_5)))
    list(
      selectizeInput(
        "componentGroup1",
        "Select LMCs:",
        choices = cmp_choices,
        selected = 1,
        multiple = TRUE
      ),
      selectizeInput(
        "componentGroup2",
        "Select LMCs to compare:",
        choices = cmp_choices,
        selected = 2,
        multiple = TRUE
      )
    )
    }
  })
  output$diffTabT<-renderUI({
    direct<-c("hypomethylated","hypermethylated")
    if (input$analysisType=="Enrichments"){
      direct<-c(direct, "differential")
    }
    selectInput("diffTableType", "Direction:", direct, selected=2)
    })

  output$region_selector<- renderUI({
    region_t=c("genes", "promoters")
    if (input$diffOutputType == "GO Enrichments" && !is.null(input$assembly) && input$assembly=="hg38"){
      region_t=c(region_t, "gencode22genes", "gencode22promoters")
    }
    if(input$diffOutputType == "LOLA Enrichments"){
      region_t=c(region_t, "tilling", "tiling200bp", "tiling500bp", "tiling1kb","tiling10kb", "cpgislands")
      if((!is.null(input$assembly)) && (input$assembly=="hg38" || input$assembly=="hg19")){
      region_t=c(region_t,"ensembleRegBuildBPall")
      }
    }
          selectInput("region_type", "Region Type:", choices=region_t, selected=1)
    })

  output$assemblySelector <- renderUI({
    result<-server_env$dataset()
    assembly<-result@parameters$ASSEMBLY
    assemblies<-list()
    assemblies[["hg38"]]<-1
    assemblies[["hg19"]]<-2
    assemblies[["mm10"]]<-3
      selectInput("assembly", "Genome Assembly:", choices=names(assemblies), selected=assemblies[[assembly]])
  })


output$lmcgoSelector<-renderUI({
  server_env$getGOEnrichmenttable()
  server_env$lmcgoSelect()
})


output$lmclolaSelector<-renderUI({
  server_env$getLOLAEnrichmenttable()
  server_env$lmclolaSelect()
})
output$analyType<-renderUI({
  atype<-c()
  if(!is.null(medecom_ref_object)){
    atype<-c(atype, "compare LMCs")
  }
  atype<-c(atype, "differential methylation", "Enrichments")
  if(PHENO_DATA_FLAG){
    atype<-c(atype, "Trait Association")
  }
  selectInput("analysisType", "Analysis:",  atype, selected=1)
  })
  output$targetVariableSelector <- renderUI({
    pheno <- server_env$getPhenoData()
    selectInput(
      "phenoTarget",
      "Select target trait:",
      choices = colnames(pheno),
      selected = 1,
      multiple = FALSE
    )

  })

  output$adjustmentVariableSelector <- renderUI({
    pheno <- server_env$getPhenoData()
    if ("phenoTarget" %in% names(input)) {
      list(
        selectizeInput(
          "phenoAdjust",
          "Select trait(s) to adjust for:",
          choices = setdiff(colnames(pheno), input$phenoTarget),
          selected = 1,
          multiple = TRUE
        ),
        if (!is.numeric(pheno[[input$phenoTarget]]))
          selectInput(
            "zeroLevel",
            "Zero level:",
            choices = unique(pheno[[input$phenoTarget]]),
            selected = 1,
            multiple = FALSE
          )
        else
          NULL
      )
    } else{
      list(NULL)
    }

  })
}
