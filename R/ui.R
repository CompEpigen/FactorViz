
#print(list.files())
#load("/home/lutsik/Documents/science/projects/heterogeneity/analysis/parameter_tuning/results.lambda.smooth.combined.cve.corr.RData")
#load("/home/lutsik/Documents/science/projects/heterogeneity/analysis/parameter_tuning/results.smooth.extended.lrange.cve.RData")
#load("collected.results.RData")

#cat(sprintf("MIN GROUPS %f", min(all_results$groups)),file='/tmp/output.shiny.test.out')
#cat(sprintf("class GROUPS %s", class(all_results$groups)),file='/tmp/output.shiny.test.out')
#cat(sprintf("class GROUPS %d, %d", ncol(all_results[["2"]]$A), nrow(all_results[["2"]]$A)),file='/tmp/output.shiny.test.out')

getFactorVizUI<-function(){
	
	tab_panels<-list()
	
	
	if(repomode){
		
#		tags$head(tags$script("
#								Shiny.addCustomMessageHandler('updateSelections',
#								function(data) {
#								var nav_ref = '#page a:contains(\"' + data.nav + '\")';
#								var tabpanel_id = data.nav == 'Runs' ? '#run_tabs' : '#explorer_tabs';
#								var tab_ref = tabpanel_id + ' a:contains(\"' + data.tab + '\")';
#								$(nav_ref).tab('show');
#								$(tab_ref).tab('show');
#								}
#								)
#							")),
		
		tab_panels[[1]]<-tags$head(tags$script("
									Shiny.addCustomMessageHandler('updateSelections',
									function(data) {
									if(data.nav){
									var nav_ref = '#page a:contains(\"' + data.nav + '\")';
									}else{
									if(data.analysisrun){
									var nav_ref='#page a:contains(\"Explorer\")';
									}else{
									var nav_ref='#page a:contains(\"Runs\")';
									}
									}
									$(nav_ref).tab('show');
									}
									)
								"))
		
		tab_panels[[2]]<-tabPanel("Runs",
							#titlePanel("List of analysis runs"),
							uiOutput("analysisList")
		)
		
	}
	
	tab_panels[[length(tab_panels)+1]]<-tabPanel('Explorer',
			
			fluidPage(
					
					titlePanel("Factorization Run Explorer"),
					if(repomode) fluidRow(
								column(width=8,
										wellPanel(
												h4("Select an analysis group:"),
												uiOutput("repoSelector"),
												h4("Select an analysis:"),
												uiOutput("runSelector")#,	
										
										)),
								column(width=2,
										wellPanel(
												#h4("Current session:"),
												shinyURL.ui(),
												br(),
												textInput("sessionName", "Session token:"),
												actionButton("saveSession", "Save session" ),
												downloadLink('sessionRdump', label = "Rdump", class = NA)
										)
								#, 
								#				wellPanel(
								#						#h4("Current session:"),
								#						
								#				)
								)
						
						),
					
					conditionalPanel(' input.runSelector !== "" ' ,
							sidebarLayout(
									conditionalPanel(' input.panel !== "Description" ', 
											sidebarPanel(width=2,
													#						
													
													conditionalPanel(' input.panel !== "Description" ' ,
															
															#selectInput('K', 'Number of components', Ks, selectize=TRUE)
															uiOutput("groupSelector")
													
													),
													
													conditionalPanel(' (input.panel !== "K selection") || (input.panel !== "Description" && input.panel !== "LMCs") || (input.panel === "LMCs" && input.componentPlotType !== "matching plot") ' ,
															
															#selectInput('K', 'Number of components', Ks, selectize=TRUE)
															uiOutput("Kselector")
													
													),
													
													
													conditionalPanel(' input.panel === "RMSEvsK" ',
															
															selectInput("KvsStat", "Statistic to plot:",
																	PERFORMANCE_MEASURES,
																	selected=2),
															uiOutput("minKselector"),
															uiOutput("maxKselector")
													
													),
													
													conditionalPanel(' input.panel === "RMSEvsK" ',
															
															checkboxInput("RMSEvsKplotAllLambdas", "Include all lambdas:", value=TRUE)
													
													),
													
													conditionalPanel(' input.panel === "RMSEvsK" && input.KvsStat === "cve" ',
															
															checkboxInput("normalizedCVE", "normalized", value=FALSE)
													
													
													),
													
													conditionalPanel(' input.panel === "RMSEvsK" && (input.KvsStat === "cve" || input.KvsStat === "rmse") ',
															checkboxInput("normalizedCVEscale", "[0,1] scale", value=FALSE)
													),
													
													conditionalPanel(' input.panel === "Performance" ',	
															
															uiOutput("performanceModeSelector"),
															checkboxInput("includeRMSE_T", "Include RMSE of T", value=FALSE),
															checkboxInput("includeDist2C_T", "Include MDC of T", value=FALSE),
															checkboxInput("includeMAE_A", "Include MAE of A", value=FALSE)
													
													),
													
													#						sliderInput('group', 'CpG subset', min=min(all_results$groups), max=max(all_results$groups), step=1, value=1),
													
													#						sliderInput('lambda', 'Lambda value (for proportion plot)', 
													#								min=1, max=length(results$lambdas), step=1, value=1, ),
													
													
													conditionalPanel(' (input.panel !== "Description" && input.panel !== "Performance" && input.panel !== "LMCs") || (input.panel === "LMCs" && input.componentPlotType !== "matching plot") ',	
															
															#selectInput('lambda', 'Lambda value', LAMBDA.IDS)
															uiOutput("lambdaSelector")
													),
													
													conditionalPanel('input.panel === "Proportions" ',	
															
															selectInput('propPlotType', 'Plot type', 
																	c("heatmap", "barplot", "lineplot", "scatterplot", "stratification plot", "correlations"), 
																	selected=1)
													
													),
													
													conditionalPanel('input.panel === "Proportions" && input.propPlotType === "lineplot" ',	
															
															checkboxInput("includeRegrProp", "Regression", value=TRUE),
															checkboxInput("includeHousemanProp", "Houseman 2012", value=TRUE),
															selectInput("propPlotLegend", "Legend", 
																	choices=c("top right"="topright",
																			"top left"="topleft",
																			"bottom right"="bottomright",
																			"bottom left"="bottomleft",
																			"none"=NA)
															),
															selectInput("sampleOrder", label="Order:", choices=c("increasing", "decreasing", "original"))
													
													),
													
													
													conditionalPanel('input.panel === "Proportions" && ( input.propPlotType === "scatterplot" ||  input.propPlotType !== "lineplot" ) ',	
															uiOutput("propMatrixSelector")
													
													),
													
													conditionalPanel('input.panel === "Proportions" && input.propPlotType === "heatmap" ',	
															
															checkboxInput("propClusterCols", "Cluster columns:", value=FALSE),
															checkboxInput("propClusterRows", "Cluster rows:", value=FALSE)
													
													
													),
													
													conditionalPanel('input.panel === "Proportions" && input.propPlotType === "heatmap" && input.propMatrix === "regression ref cmp" ',	
															
															uiOutput("componentSelectorRef")
													
													),
													
													conditionalPanel('input.panel === "Proportions" && input.propPlotType === "stratification plot" ',	
															
															checkboxInput("includeRegressionLine", "Include regression line:", value=FALSE),
															checkboxInput("vertPlotLabs", "Vertical labels:", value=FALSE)
													
													),
													
													conditionalPanel(' ( input.panel === "LMCs" && input.componentPlotType === "histogram" ) || ( input.panel === "LMCs" && input.componentPlotType === "scatterplot all" ) || (input.panel === "Proportions" && input.propPlotType !== "heatmap") ',	
															
															uiOutput("componentSelector")
													
													),
													
													
													conditionalPanel(' input.panel === "Proportions" && input.propPlotType !== "heatmap"',	
															
															uiOutput("refProfileSelector")
													
													),
													
													
													conditionalPanel('input.panel === "LMCs" ',	
															
															selectInput('componentPlotType', 'Plot type', 
																	c("dendrogram","matching plot", "distance to center", "extremality", "heatmap", "mds plot", "similarity graph", "histogram", "scatterplot all","scatterplot matching","scatterplot avg matching", "locus plot"), 
																	selected=1)
													
													),
													
													conditionalPanel('input.panel === "LMCs" && (input.componentPlotType === "heatmap" || input.componentPlotType === "mds plot" || input.componentPlotType === "dendrogram"  || input.componentPlotType == "similarity graph" ) ',
															
															checkboxInput("cgVarSubset", "Select top-variable CpGs:", value=FALSE)
													
													),
													
													
													conditionalPanel('input.panel === "LMCs" && (input.componentPlotType === "heatmap" || input.componentPlotType === "mds plot" || input.componentPlotType === "dendrogram" || input.componentPlotType === "similarity graph") && input.cgVarSubset ',
															
															uiOutput("topSDcgsSelector")
													),
													
													conditionalPanel(' input.panel === "LMCs" && (input.componentPlotType === "heatmap" || input.componentPlotType === "mds plot" || input.componentPlotType === "similarity graph" || input.componentPlotType === "dendrogram" )',	
															
															selectInput("mdsDist", "Distance:", c("euclidean", "angular", "correlation"), selected=1)
													
													),
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType === "mds plot" ',	
															
															selectInput("mdsInclude", "Include data:", c(
																			#"components only"="comp", 
																			"references"="ref", "all data"="data"), selected=1)
													
													),
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType === "mds plot" && input.mdsInclude === "data"',	
															
															selectInput("mdsSampleNames", "Sample names:", c(
																			#"components only"="comp", 
																			"hide"="hide", "show"="show"), selected=1)
													
													),
													
													conditionalPanel(' (input.panel === "LMCs" && input.componentPlotType === "mds plot" && input.mdsInclude === "data") || (input.panel === "Proportions" && input.propPlotType !== "lineplot" ) ',	
															
															uiOutput("sampleColorSelector")
													
													),
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType !== "heatmap" && input.componentPlotType !== "mds plot" && input.componentPlotType !== "dendrogram" && input.componentPlotType !== "similarity graph" && input.componentPlotType !== "histogram" && input.componentPlotType !== "locus plot" ',	
															
															selectInput("scatterType", "Scatterplot", c("smoothed","ordinary"), selected=1)
													
													),
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType !== "heatmap" && input.componentPlotType !== "mds plot" && input.componentPlotType !== "dendrogram" && input.componentPlotType !== "histogram" && input.scatterType === "ordinary" ',	
															
															uiOutput("pointColorSelector"),
															uiOutput("pointFilter"),
															uiOutput("CGcategorySubsetSelector")
													
													),
													
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType === "locus plot" ',	
															
															
															selectInput("locusType", "Locus:", c("from gene sets"="predefined", "arbitrary"="arbitrary"))
													
													),
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType === "locus plot" && input.locusType === "predefined" ',	
															
															uiOutput("geneSetSelector"),
															uiOutput("locusSelector")
													
													),
													
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType === "locus plot" && input.locusType === "arbitrary" ',	
															
															textInput("locusName", "Name:", ""),
															textInput("locusChr", "Chromosome:", ""),
															textInput("locusStart", "Start:", ""),
															textInput("locusEnd", "End:", ""),
															selectInput("locusStrand", "Strand:", c("+","-"))
													
													),
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType === "locus plot" ',	
															
															
															#									numericInput("upstreamMinus", "Flank upstream (bp):", 10000, min=-1000000, max=1000000),
															#									numericInput("downstreamPlus", "Flank downstream (bp):", 10000, min=-1000000, max=1000000)
															sliderInput("upstreamMinus", "Flank upstream (bp):", 10000, min=-100000, max=100000, step=10),
															sliderInput("downstreamPlus", "Flank downstream (bp):", 10000, min=-100000, max=100000, step=10),
															
															checkboxInput("locusIncludeTref", "Include reference profiles (Tstar)", value=TRUE),
															checkboxInput("locusIncludeD", "Include target profiles (D)", value=TRUE)
													
													),
													
													conditionalPanel(' input.panel === "LMCs" && input.componentPlotType === "similarity graph" ',
															
															sliderInput('minGraphSimilarity', 'Minimal similarity', min=0, max=1, step=0.01, value=0)
													),
													
													conditionalPanel(' input.panel === "Performance" && input.performanceMode === "lineplots" ',	
															
															selectInput('lambdaScale', "Lambda scale", c("native", "logarithmic"), selected=1),
															uiOutput("minLambdaSelector"),
															uiOutput("maxLambdaSelector")
													
													#selectInput('lambdaMin', 'Minimum lambda', all_results$lambdas),
													#selectInput('lambdaMax', 'Maximum lambda', all_results$lambdas, selected=all_results$lambdas[length(all_results$lambdas)])#,
													),
													
													
													conditionalPanel(' input.panel === "Meta-analysis" ',
															
															selectInput("analysisType", "Analysis:", 
																	c(if(repomode || !is.null(medecom_ref_object)) "compare LMCs" else NULL, 
																			"differential methylation", "phenotype modeling"), selected=1)
													),
													
													
													conditionalPanel(' input.panel === "Meta-analysis" && input.analysisType === "compare LMCs" ',
															
															wellPanel(
																	#h5("Select an analysis group:"),
																	#uiOutput("repoSelector"),
																	h5("Select a reference run:"),
																	uiOutput("compareRunSelector"),
																	uiOutput("Kselector_ref"),
																	uiOutput("lambdaSelector_ref")
															
															),
															
															selectizeInput('compareMatrices', 'Select matrices:', 
																	choices = list(
																			"Current run" = c(`LMCs (\\hat{T})` = 'That', `Data` = 'D', `Reference (T^star)` = 'Tstar'),
																			"Reference run" = c(`LMCs (\\hat{T})` = 'refThat', `Data` = 'refD', `Reference (T^star)` = 'refTstar')
																	), multiple = TRUE),
															
															selectInput("comparativePlotType", "Output type:", c("dendrogram","heatmap","correlation heatmap"), selected=1),
															uiOutput("topSDcgsSelectorCompare"),
															radioButtons("SDCompareMatrices", "Calculate SD on:",
																	c(`All`= "All", `Estimates (\\hat{T})` = 'That', `Data` = 'D', `Reference (T^star)` = 'Tstar')),
															uiOutput("analysisTokensInput")
													
													),
													
													conditionalPanel(' input.panel === "Meta-analysis" && input.analysisType === "differential methylation" ',
															
															uiOutput("dmCGComponentSelector"),
															selectInput("diffTableType", "Direction:", c("hypermethylated","hypomethylated"), selected=1),
															sliderInput('dmr_threshold', 'Threshold', min=0.0, max=1.0, step=0.01, value=1.0),
															selectInput('diffOutputType', "Output type:", c("Table", "GREAT enrichments"), selected=1)
													
													),
													
													conditionalPanel(' input.panel === "Meta-analysis" && input.analysisType === "differential methylation" && input.diffOutputType === "Table" ',
															
															actionButton('locusPlotSelected', "Locus plot")
													
													),
													
													conditionalPanel(' input.panel === "Meta-analysis" && input.analysisType === "differential methylation" && input.diffOutputType === "GREAT enrichments" ',
															
															selectInput('GREATresults', "Show:", c("start screen", "region-gene associations (plot)", "region-gene associations (table)",
																			"enriched ontologies (table)", "enriched ontologies (plot)", "region-gene associations for term")),
															uiOutput("GREAToptionsSelector"),
															actionButton('GREATsubmitQuery', "Submit GREAT query")
													
													),
													
													conditionalPanel(' ( input.panel === "LMCs" && input.componentPlotType === "heatmap" ) || ( input.panel === "LMCs" && input.componentPlotType === "scatterplot all" ) || ( input.panel === "LMCs" && input.componentPlotType === "scatterplot matching" ) || ( input.panel === "LMCs" && input.componentPlotType === "scatterplot avg matching" ) || ( input.panel === "LMCs" && input.componentPlotType === "dendrogram" ) || ( input.panel === "LMCs" && input.componentPlotType === "similarity graph" )|| ( input.panel === "Proportions" && input.propPlotType === "correlations" ) || ( input.panel === "Meta-analysis" && input.analysisType === "compare LMCs" && input.comparativePlotType !== "heatmap" ) ',
															
															checkboxInput("correlationCentered", "Center matrices", value=FALSE)
													),	
													
													conditionalPanel(' input.panel === "Meta-analysis" && input.analysisType === "phenotype modeling" ',
															
															selectInput("phenoModelOutput", "Output:", choices=c("summary plot","single case")),
															uiOutput("targetVariableSelector"),
															uiOutput("adjustmentVariableSelector"),
															selectInput("discardLMC", "Which LMC to discard:", choices=c("largest", "smallest"), selected=1),
															selectInput("modelPval", "p-value:", choices=c("overall", "minimal"), selected=1)
													
													),
													
													checkboxInput("addPlotTitle", "Add plot titles", value=TRUE)
											)
									),
									
									mainPanel(
											tabsetPanel(id = 'panel',
													tabPanel('Description',
															#wellPanel(
															#textOutput('AnalysisRunName', container=div)
															
															#)
															uiOutput("AnalysisRunDescriptionHeader")
															,
															tableOutput('AnalysisRunParameterTable')#,
													#wellPanel(
													#	uiOutput('downloadPanel')
													#)
													),
													tabPanel('K selection',
															value='RMSEvsK', 
															plotOutput('RMSEvsKplot'
																	,height = "450px"
																	,width = "600px"
															),
															br(),
															#uiOutput('KselectionPanel',)
															downloadLink('RMSEvsKplotPDF', label = "PDF", class = NA)
													),	
													
													tabPanel('Lambda selection',
															value='Performance', 
															#plotOutput('lineplot', 
															#		height = "800px",
															#		width = "600px")
															uiOutput('performancePanel')
													),
													tabPanel('LMCs',
															#plotOutput('componentPlot',
															#		height = "1000px",
															#		width = "1000px")
															
															uiOutput('componentsPanel') 
													),
													
													tabPanel('Proportions', 
															plotOutput('proportionplot', 
																	height = "500px",
																	width = "500px"),
															br(),
															downloadLink('proportionPlotPDF', label = "PDF", class = NA)),
													
													tabPanel('Meta-analysis', 
															uiOutput('metaanalysisPanel')),
													
													
													tabPanel('Downloads',
															uiOutput('downloadPanel'))
											)
									)
							
							#	fluidRow(
							#	column(7,plotOutput('lineplot')),
							#	column(5,plotOutput('proportionplot'))
							#)
							
							)#end sidebar layout
					)#end enclosing of sidebar layout
			#		,
			#		wellPanel(
			#				h4("Current session:"),
			#				shinyURL.ui()
			#		)
			)#end fluidpage
	)# end tabPanel explorer	
	
	if(repomode) {
		tab_panels[[length(tab_panels)+1]]<-tabPanel("Sessions", uiOutput("sessionsPanel"))
		
	}
	
	tab_panels$title<-"FactorViz"
	tab_panels$id<-"page"
		
	
	
	shinyUI(
			#pageWithSidebar(
			do.call(navbarPage, tab_panels)
	  #end navbarpage
	)
}