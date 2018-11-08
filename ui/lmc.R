lmc_tab<-tabPanel("LMCs", id="LMC", 
                  sidebarPanel(
                    width = 2,
                    uiOutput("groupSelector_3"),
                    uiOutput("Kselector_3"),
                    conditionalPanel('input.componentPlotType !== "matching plot" ',	
                                     uiOutput("lambdaSelector_3")
                    ),
                    uiOutput('componentPlotT'),
                    conditionalPanel('input.componentPlotType === "histogram" || 
                                     input.componentPlotType === "scatterplot all" ',	
                                     uiOutput("componentSelector")
                    ),
                    conditionalPanel('input.componentPlotType === "heatmap" || 
                                     input.componentPlotType === "mds plot" || 
                                     input.componentPlotType === "dendrogram"  || 
                                     input.componentPlotType == "similarity graph" ',
                                     checkboxInput("cgVarSubset", "Select top-variable CpGs:", value=FALSE)
                    ),
                    conditionalPanel('(input.componentPlotType === "heatmap" ||
                                     input.componentPlotType === "mds plot" || 
                                     input.componentPlotType === "dendrogram" || 
                                     input.componentPlotType === "similarity graph") && input.cgVarSubset ',
                                     uiOutput("topSDcgsSelector")
                    ),
                    conditionalPanel('input.componentPlotType === "heatmap" ||
                                     input.componentPlotType === "mds plot" ||
                                     input.componentPlotType === "similarity graph" ||
                                     input.componentPlotType === "dendrogram" ',
                                     selectInput("mdsDist", "Distance:", c("euclidean", "angular", "correlation"), selected=1)
                    ),
                    conditionalPanel('input.componentPlotType === "mds plot" ',
                                     selectInput("mdsInclude", "Include data:", c("references"="ref", "all data"="data"), selected=1)
                    ),
                    conditionalPanel('input.componentPlotType === "mds plot" &&
                                     input.mdsInclude === "data"',
                                     selectInput("mdsSampleNames", "Sample names:", c("hide"="hide", "show"="show"), selected=1)
                                     
                    ),
                    conditionalPanel('input.componentPlotType === "mds plot" &&
                                     input.mdsInclude === "data"',	
                                     uiOutput("sampleColorSelector")
                                     
                    ),
                    
                    conditionalPanel('input.componentPlotType !== "heatmap" &&
                                     input.componentPlotType !== "mds plot" && 
                                     input.componentPlotType !== "dendrogram" && 
                                     input.componentPlotType !== "similarity graph" && 
                                     input.componentPlotType !== "histogram" && 
                                     input.componentPlotType !== "locus plot" ',	
                                     selectInput("scatterType", "Scatterplot", c("smoothed","ordinary"), selected=1)
                    ),
                    conditionalPanel('input.componentPlotType !== "heatmap" && 
                                     input.componentPlotType !== "mds plot" && 
                                     input.componentPlotType !== "dendrogram" && 
                                     input.componentPlotType !== "histogram" && 
                                     input.scatterType === "ordinary" ',	
                                     uiOutput("pointColorSelector"),
                                     uiOutput("pointFilter"),
                                     uiOutput("CGcategorySubsetSelector")
                                     
                    ),
                    conditionalPanel('input.componentPlotType === "locus plot"',
                                      selectInput("locusType", "Locus:", c("from gene sets"="predefined", "arbitrary"="arbitrary"))
                    ),
                    conditionalPanel('input.componentPlotType === "locus plot" &&
                                     input.locusType === "predefined" ',	
                                     uiOutput("geneSetSelector"),
                                     uiOutput("locusSelector")
                    ),
                    conditionalPanel('input.componentPlotType === "locus plot" &&
                                     input.locusType === "arbitrary" ',	
                                     textInput("locusName", "Name:", ""),
                                     textInput("locusChr", "Chromosome:", ""),
                                     textInput("locusStart", "Start:", ""),
                                     textInput("locusEnd", "End:", ""),
                                     selectInput("locusStrand", "Strand:", c("+","-"))
                    ),
                    conditionalPanel('input.componentPlotType === "locus plot"',
                                     sliderInput("upstreamMinus", "Flank upstream (bp):", 10000, min=-100000, max=100000, step=10),
                                     sliderInput("downstreamPlus", "Flank downstream (bp):", 10000, min=-100000, max=100000, step=10),
          
                                     checkboxInput("locusIncludeTref", "Include reference profiles (Tstar)", value=TRUE),
                                     checkboxInput("locusIncludeD", "Include target profiles (D)", value=TRUE)
                    ),
                    
                    conditionalPanel('input.componentPlotType === "similarity graph" ',
                                     sliderInput('minGraphSimilarity', 'Minimal similarity', min=0, max=1, step=0.01, value=0)
                    ),
                    conditionalPanel('input.componentPlotType === "heatmap" ||
                                     input.componentPlotType === "scatterplot all" ||
                                     input.componentPlotType === "scatterplot matching" ||
                                     input.componentPlotType === "scatterplot avg matching" ||
                                     input.componentPlotType === "dendrogram" ||
                                     input.componentPlotType === "similarity graph" ',
                                     checkboxInput("correlationCentered", "Center matrices", value=FALSE)
                    ),	
                    checkboxInput("addPlotTitle", "Add plot titles", value=TRUE)
                  ),
                  mainPanel(
                    uiOutput('componentsPanel')
                  )
)