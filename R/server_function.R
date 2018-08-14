FactorViz_serverFunc<-function(input, output, object=NULL, ref_object=NULL, dataset=NULL, session=NULL, runPart=NULL) {

	if(!(exists("runPart") && !is.null(runPart))){
		values<-reactiveValues(change=FALSE)
	}else{
		values<-list()
	}
	
	if(!(exists("runPart") && !is.null(runPart))){
		shinyURL.server(session)
	}
		
	if(exists("runPart") && !is.null(runPart)){
		reactive<-function(expr, quoted=TRUE){
			f<-function(input=get("input", parent.frame())){}
			body(f)<-expr
			environment(f)<-parent.frame()
			f
		}
		withProgress<-function(message="", expr={}, value=0){
			print(message)
			eval(expr)
		}
		incProgress<-function(d, detail){
			print(detail)
		}
		observe<-function(expr, quoted=TRUE){
			return()
		}
		
		renderUI<-function(expr){
			return()
		}
	}
	
	### update tab selection

	observe(quote({
				data <- parseQueryString(session$clientData$url_search)
				session$sendCustomMessage(type='updateSelections', data)
			#	updateSelectInput(session, 'beverage', selected=data$beverage)
			}), quoted=TRUE)
	output$plot_t_LMC<-renderUI({
				if(repomode){
					plot_t=c("dendrogram", "distance to center", "extremality", "heatmap", "similarity graph", "scatterplot all","scatterplot matching","scatterplot avg matching")
				} else {
					plot_t=c("dendrogram", "heatmap")
				} 
				if (!is.null(input$k) && input$K>2){
					plot_t=c(plot_t, "mds")
				}		
				selectInput('componentPlotType', 'Plot type', 
				choices=plot_t,
				selected=input$componentPlotType)
		})
	output$analysisList<-renderUI({
				withProgress(message="Preparing list of available analyses...",
						{
							lapply(names(repo_dir_list), function(repo_dir_name)
								{
								incProgress(1/length(repo_dir_list),
										detail=sprintf("Loading %s...",repo_dir_name))
								getRepoSummaryElement(repo_dir_name)
							})
						}, value=0)
			})
	
	getRepoSummaryElement<-function(repo_dir_name){
		
		element<-list()
		element[[1]]<-h4(repo_dir_name)
		element[[2]]<-div(
				renderTable({getRepoTable(repo_dir_name)},
				rownames=FALSE,
				sanitize.text.function = function(x) x),
		style = 'width:2000px;'
		#, class='collapse'
		)
		element
	}
	
	getAnalysisURL<-function(run_list, repodir){
		sprintf("%s?nav=Explorer&repodir=%s&analysisrun=%s", BASE_URL, repodir, run_list[["ANALYSIS"]])
	}
	getRepoTable<-function(repo_dir_name){
		runs<-scan_run_repo(repo_dir_list[[repo_dir_name]])
		table_data<-lapply(runs, "[", names(FactorViz:::RUN_LIST_COLUMNS))
		table_data<-lapply(table_data, unlist)
		table_data<-do.call("rbind", table_data)
		colnames(table_data)<-FactorViz:::RUN_LIST_COLUMNS
		table_data<-as.data.frame(table_data)
		URLs<-sapply(runs, getAnalysisURL, repodir=repo_dir_list[[repo_dir_name]])
		#URLs<-paste0("<a href='", URLs,  "' target='_blank'>",names(runs),"</a>")
		URLs<-paste0("<a href='",URLs, "' >",names(runs),"</a>")
		table_data$ID<-URLs
		table_data<-table_data[,c("ID",FactorViz:::RUN_LIST_COLUMNS)]
		rownames(table_data)<-NULL
		table_data
	}
	getRuns<-reactive(quote({
						if(repomode){
							scan_run_repo(input$repodir)
						}else{
							list()
						}
					}),quoted=TRUE)
	
	output$repoSelector<-renderUI({
				selectInput('repodir', NULL, repo_dir_list, selectize=TRUE, width="200px", selected=1)
			})
	
	
	output$runSelector<-renderUI({
				#wellPanel(
				runs<-names(getRuns())
				names(runs)<-paste(seq_len(length(runs)), runs, sep=". ")
				selectInput('analysisrun', NULL, runs, selectize=TRUE, width="750px", selected=1)
				#)		
			})
	
	dataset <- reactive(quote({
					if(repomode){
						results<-readRDS(file.path(getRuns()[[input$analysisrun]][["run.dir"]], "collected.results.as.medecomset.RDS"))
					}else{
						results<-medecom_object
					}
					results
					}),quoted=TRUE)
	
	if(!is.null(runPart) && runPart=="dataset"){
		return(dataset())
	}
	
	dataset_ref <- reactive(quote({
					if(repomode){
						results<-readRDS(file.path(getRuns()[[input$analysisrun_ref]][["run.dir"]], "collected.results.as.medecomset.RDS"))
					}else{
						results<-medecom_ref_object
					}
						results
					}),quoted=TRUE)
	
	if(!is.null(runPart) && runPart=="dataset"){
		return(dataset())
	}
	
	getTrueT <- reactive(quote({
						lenv<-new.env()
						#cat(sprintf("run data dir %s\n", as.character(getRuns()[[input$analysisrun]][["proportion.info.present"]])),file='/tmp/output.shiny.test.out')
						if(repomode){
							load(file.path(getRuns()[[input$analysisrun]][["data.dir"]], "trueT.RData"))
							get("trueT", envir=lenv)
						}else{
							true_T_matrix
						}
					}),quoted=TRUE)
	
	if(!is.null(runPart) && runPart=="Tstar"){
		return(getTrueT())
	}
	
	getRefTrueT <- reactive(quote({
						lenv<-new.env()
						#cat(sprintf("run data dir %s\n", as.character(getRuns()[[input$analysisrun]][["proportion.info.present"]])),file='/tmp/output.shiny.test.out')
						if(repomode){
							load(file.path(getRuns()[[input$analysisrun_ref]][["data.dir"]], "trueT.RData"))
							get("trueT", envir=lenv)
						}else{
							trueT_matrix_ref
						}
					}),quoted=TRUE)
	
	if(!is.null(runPart) && runPart=="Tstar"){
		return(getTrueT())
	}
	
	getTrueA <- reactive(quote({
						#cat(sprintf("trueA PRESENT %s\n", as.character(getRuns()[[input$analysisrun]][["proportion.info.present"]])),file='/tmp/output.shiny.test.out')
						if(repomode){
							if(getRuns()[[input$analysisrun]][["proportion.info.present"]]){
								#if(file.exists(file.path(getRuns()[[input$analysisrun]][["run.dir"]], "trueA.RData"))){
								lenv<-new.env()
								load(file.path(getRuns()[[input$analysisrun]][["data.dir"]], "trueA.RData"))
								get("trueA", envir=lenv)
							}else{
								NULL
							}
						}else{
							true_A_matrix
						}		
					}),quoted=TRUE)
	
	getSampleSubset<-reactive(quote({
						dataset()@parameters$sample_subset
					}), quoted=TRUE)
	
	getRefSampleSubset<-reactive(quote({
						dataset_ref()@parameters$sample_subset
					}), quoted=TRUE)
	
	getRegressionA <-reactive(quote({
						
						gr<-as.integer(input$cg_group)
						meth.data<-getMethData()
						trueT<-getTrueT()
						sample_subset<-getSampleSubset()
						#cat(sprintf("TRUEA THERE %s\n", as.character(!is.null(trueA))), file='/tmp/output.shiny.test.out')
						
						ind<-getCGsubset()
						#regrA<-MeDeCom:::factorize.regr(meth.data[ind,sample_subset],trueT[ind,])[["A"]]
						regrA<-MeDeCom:::factorize.regr(meth.data[ind,sample_subset],trueT[ind,])[["A"]]
						rownames(regrA)<-colnames(trueT)
						regrA
						
					}),quoted=TRUE)
	
	
	getRefCmpRegressionA <-reactive(quote({
						
						if("analysisrun_ref" %in% names(input)){
							#isolate({
								gr<-as.integer(input$cg_group)
								cat("--------------------------------groups" , gr, "---------------------------------------------")
								meth.data<-getMethData()
								trueT<-getTrueT()
								sample_subset<-getSampleSubset()
								
								ll_ref<-as.integer(input$lambda_ref)
								#cat(sprintf("TRUEA THERE %s\n", as.character(!is.null(trueA))), file='/tmp/output.shiny.test.out')
															
								ind<-getCGsubset()
								
								ind_ref<-getCGsubsetRef()
								
								ind_common<-intersect(ind, ind_ref)
								
								cg_map<-match(ind_common,ind)
								cg_map_ref<-match(ind_common,ind_ref)
								
								regrA<-MeDeCom:::factorize.regr(
										meth.data[ind[cg_map],sample_subset], 
										dataset_ref()@outputs[[input$K_ref]]$T[[gr,ll_ref]][cg_map_ref,as.integer(input$component_ref)])[["A"]]
								rownames(regrA)<-as.character(1:nrow(regrA))
								regrA	
							#})
						}
						
					}),quoted=TRUE)
	
	getAhouseman2012 <- reactive(quote({
						#cat(sprintf("trueA PRESENT %s\n", as.character(getRuns()[[input$analysisrun]][["proportion.info.present"]])),file='/tmp/output.shiny.test.out')
						if(getRuns()[[input$analysisrun]][["houseman.A.present"]]){
							#if(file.exists(file.path(getRuns()[[input$analysisrun]][["run.dir"]], "trueA.RData"))){
							lenv<-new.env()
							sample_subset<-getSampleSubset()
							load(file.path(getRuns()[[input$analysisrun]][["data.dir"]], "Ahouseman2012.RData"))
							get("Ahouseman2012", envir=lenv)[,sample_subset]
						}else{
							NULL
						}
					}),quoted=TRUE)

#	getAhouseman2016 <- reactive(quote({
#						#cat(sprintf("trueA PRESENT %s\n", as.character(getRuns()[[input$analysisrun]][["proportion.info.present"]])),file='/tmp/output.shiny.test.out')
#						if(getRuns()[[input$analysisrun]][["houseman.A.2016.present"]]){
#							#if(file.exists(file.path(getRuns()[[input$analysisrun]][["run.dir"]], "trueA.RData"))){
#							lenv<-new.env()
#							sample_subset<-getSampleSubset()
#							load(file.path(getRuns()[[input$analysisrun]][["data.dir"]], "Ahouseman2016.RData"))
#							get("Ahouseman2016", envir=lenv)[,sample_subset]
#						}else{
#							NULL
#						}
#					}),quoted=TRUE)

	getAhouseman2016 <- reactive(quote({
						
						require(RefFreeEWAS)
						
						HOUSEMAN_2016_TOP=20000L
						
						meth.data<-getMethData()[getCGsubset(),getSampleSubset()]
						sds<-apply(meth.data, 1, sd)
						top_cgs<-order(sds, decreasing=TRUE)[1:min(length(sds),HOUSEMAN_2016_TOP)]
						mdh<-meth.data[top_cgs,]
						
						res<-RefFreeCellMix(mdh, K=as.integer(input$K), Yfinal=meth.data);
						
						Thouseman2016=res$Mu
						
						perm<-AHMETPRO:::match.components(getTrueT()[getCGsubset(),], Thouseman2016)
						
						Ahouseman2016=t(res$Omega[,perm])
						colnames(Ahouseman2016)<-NULL
						if(is.null(runPart) || runPart!="houseman2016"){
							Ahouseman2016
						}else{
							Thouseman2016=res$Mu[,perm]
							list(That=Thouseman2016, Ahat=Ahouseman2016, top_cgs=top_cgs)
						}
						
						
					}),quoted=TRUE)
	
	if(!is.null(runPart) && runPart=="houseman2016"){
		return(getAhouseman2016())
	}
	
	
	getInputDataset<-reactive(quote({
						
						if(repomode){
							readRDS(load(file.path(getRuns()[[input$analysisrun]][["data.dir"]], "rnb.set.RData")))
						}else{
							input_object
						}
					}), quoted=TRUE)
	
	getMethData <-reactive(quote({
						getInputDataset()$meth.data
					}),quoted=TRUE)
	
	getRefMethData <-reactive(quote({
						getRefInputDataset()$meth.data
					}),quoted=TRUE)
	
	getSampleNames <- reactive(quote({
						getInputDataset()$sample.names
					}),quoted=TRUE)
	
	getRefSampleNames <- reactive(quote({
						getRefInputDataset()$sample.names
					}),quoted=TRUE)
	
	getPhenoData<-reactive(quote({
#						if(file.exists(file.path(getRuns()[[input$analysisrun]][["data.dir"]], "pheno.RData"))){
#							lenv<-new.env()
#							load(file.path(getRuns()[[input$analysisrun]][["data.dir"]], "pheno.RData"))
#							get("pheno.data", envir=lenv)
#						}else{
#							NULL
#						}
						getInputDataset()$pheno.data
					}),quoted=TRUE)
	
	if(!is.null(runPart) && runPart=="phenoData"){
		return(getPhenoData())
	}
	
	getCGsubset<-reactive(quote({
							dataset()@parameters$cg_subset_lists[[as.integer(input$cg_group)]]
					}),quoted=TRUE)
	
	getCGsubsetRef<-reactive(quote({
							dataset_ref()@parameters$cg_subset_lists[[as.integer(input$cg_group_ref)]]
					}),quoted=TRUE)
	
	getGeneSets<-reactive(quote({
						
						if(repomode){
							lenv<-new.env()
							source(sprintf("%s/gene_sets.RDump",  
											GLOBAL_DATA_DIR
									), local=lenv)
							
							output<-list()
							for(gs in DATASET_GENE_SETS[[getRuns()[[input$analysisrun]][["DATASET"]]]]){
								output<-c(output, get(gs, lenv))
							}
							output
						}else{
							gene_sets_object
						}
						
					}),
			
			quoted=TRUE)
	
	getCGAnnot<-reactive(quote({
						cg_annot_object
					}),quoted=TRUE)
	
	getGeneAnnot<-reactive(quote({
						gene_annot_object
					}),quoted=TRUE)
	cat(file = stderr(), prepared_annotation[["cat_list"]], 'Hello\n')
	getCGcategories<-reactive(quote({
						
						if(repomode){
							readRDS(file=sprintf("%s/%s_categories.RDS",  
											GLOBAL_DATA_DIR, DATASET_META_INFO[[getRuns()[[input$analysisrun]][["DATASET"]]]][["cg_annot"]]
									))
						}else{
							prepared_annotation[["cat_list"]]
						}
					}),quoted=TRUE)
	
	getCGcatAssignments<-reactive(quote({
						prepared_annotation[["cat_inf_vs"]]
					}),quoted=TRUE)
	
	getCGquantFeatures<-reactive(quote({
						prepared_annotation[["features"]]
					}), quoted=TRUE)
	
	getCGquantFeatureSettings<-reactive(quote({
						prepared_annotation[["settings"]]
					}),quoted=TRUE)
	
	getTTandTrefSDranking<-reactive(quote({
				
				results<-dataset()
				#gr<-1
				gr<-as.integer(input$cg_group)
				ll<-as.integer(input$lambda)
				cmp<-as.integer(input$component)
				ind<-getCGsubset()
				trueT<-getTrueT()
				Tref<-trueT[ind,]
				
				sds<-apply(cbind(Tref, results@outputs[[input$K]]$T[[gr,ll]]), 1, sd)
				order(sds, decreasing = TRUE)
				
			}), quoted=TRUE)
	
	if(!is.null(runPart) && runPart=="houseman2016"){
		return(getAhouseman2016())
	}
	
	if(!is.null(runPart) && runPart=="trueT"){
		return(getTrueT())
	}

	getAnalysisName<-function(){
		an<-dataset()@dataset_info$analysis_name
		if(is.null(an)){
			an<-"Unnamed analysis"
		}
		an
	}
	#### automated GUI elements
	output$AnalysisRunName<-renderText({
				getAnalysisName()
			})
	
	output$AnalysisRunDescriptionHeader<-renderUI({
				wellPanel(
						#strong("Analysis Run:"),
						h4(getAnalysisName())
				)
			})
	
	#Description table	
	output$AnalysisRunParameterTable<-renderTable({

				output<-list()
				#cat(sprintf("NAMES"), file='/tmp/output.shiny.test.out')	
				#for(name in names(getRuns()[[input$analysisrun]])){
				print("Starting table")
				#hier greift man noch auf analysisrun zu !!
				results<-dataset()
				
				#TODO: M und N in  collected.results schreiben!! hier einfach werte gesetzt 
				#results <-MeDeComSet(outputs=results$outputs, parameters=results$parameters, dataset_info=list(m=100, n=100))
				print("...............................MeDeComSet...........................")
				print(results)
				print("...............................MeDeComSet_ende...........................")
				for(name in names(FactorViz:::ANALYSIS_META_INFO[["analysis_params"]])){
					#print(name)
					display_name<-FactorViz:::ANALYSIS_META_INFO[["analysis_params"]][name]
					#cat(sprintf("NAME %s", name), file='/tmp/output.shiny.test.out', append=TRUE)
#					if(name %in% names(getRuns()[[input$analysisrun]])){
					if(name %in% names(results@parameters)){
						val<-results@parameters[[name]]
						#if(name %in% c("LAMBDA_GRID")){
						if(name %in% c("lambdas")){
							val<-sort(val)						
						}
			#			if(name %in% c("cg_subsets")){
			#				print("hihi")
			#			val<-paste(names(val), collapse=", ")
			#			}
						if(is.integer(val)){
							val<-as.character(val)
						}else{
							if(name %in% names(FactorViz:::ANALYSIS_META_INFO[["param_extensions"]])){
								val_ext<-FactorViz:::ANALYSIS_META_INFO[["param_extensions"]][[name]][val]
								if(!is.na(val_ext)){
									val<-val_ext
								}
							}
#							else if(name %in% c("DATASET")){
#								val<-DATASET_META_INFO[[val]][["full_name"]]
#							}else if(name %in% c("DATA_SUBSET")){
#								val<-DATASET_META_INFO[[getRuns()[[input$analysisrun]][["DATASET"]]]][["data_subsets"]][val]
#							}
						}
					}else if(name %in% "NO_OF_SAMPLES"){
						val<-as.character(ncol(getMethData()))
					}else if(name %in% "REFERENCE_PROFILES"){
						val<-paste(colnames(getTrueT()), collapse=", ")
#					}else if(name %in% "REFERENCE_PROFILES_SOURCE"){
#						ds<-DATASET_META_INFO[[getRuns()[[input$analysisrun]][["DATASET"]]]][["ref_data_set"]]
#						val<-DATASET_META_INFO[[ds]][["full_name"]]
					}else{
						val=""
					}
					#cat(sprintf("NAME %s", name), file='/tmp/descr.table.output.shiny.test.out', append=TRUE)	
					if(length(val)==0){
						val=""
					}
					if(length(val)>1){
						output[[name]]<-c(display_name, paste(val, collapse=", "))
					}else if(length(val)==1 && val!=""){
						output[[name]]<-c(display_name, paste(val, collapse=", "))
					}
				}
				
				
				table<-do.call("rbind", output)
				rownames(table)<-NULL
				colnames(table)<-c("Parameter", "Value")
				table
			}, width=1000, include.rownames=FALSE)
	
	
	#if(is.null(runPart)){
	output$groupSelector<-renderUI({
			#results<-readRDS(file.path(getRuns()[[input$analysisrun]][["run.dir"]], "collected.results.RDS"))
			results<-dataset()
			#results <-MeDeComSet(outputs=results$outputs, parameters=results$parameters, dataset_info=list(m=100, n=100))
			
				
				#cat(sprintf("VAR EXISTS %s", "cg_subsets" %in% names(getRuns()[[input$analysisrun]])),file='/tmp/output.shiny.test.out')
				
				gr_lists<-results@parameters$cg_subsets
				
				GROUPS<-1:length(gr_lists)
				if(is.null(names(gr_lists))){
					names(GROUPS)<-sapply(gr_lists, paste, collapse="_")
				}else{
					names(GROUPS)<-names(gr_lists)
				}
				
#				for(gr in GROUPS){
					#cat(sprintf("GROUP PATHS %s", .libPaths()),file='/tmp/output.shiny.test.out')
					#cat(sprintf("GROUP %d, NAME %s\n", gr, names(GROUPS[gr])) ,file='/tmp/output.shiny.test.out', append=TRUE)
#				}
				
				selectInput('cg_group', 'Technical CpG subset:', GROUPS, selectize=TRUE)
			})
	

	
	
	output$Kselector<-renderUI({
				#Ks<-dataset()[["Ks"]]
				Ks<-dataset()@parameters$Ks
				selectInput('K', 'Number of LMCs (k)', Ks, selectize=TRUE)
			})
	

	output$include_type<-renderUI({
				if (repomode){
				checkboxInput("includeRMSE_T", "Include RMSE of T", value=FALSE)
				checkboxInput("includeDist2C_T", "Include MDC of T", value=FALSE)
				checkboxInput("includeMAE_A", "Include MAE of A", value=FALSE)
				}
			})	
	output$performanceModeSelector<-renderUI({
				selectInput('performanceMode', 'Show results as:', c("lineplots","table"), selectize=TRUE)
			})
	
	output$propMatrixSelector<-renderUI({
				
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
				
				if(repomode){
					
					if(getRuns()[[input$analysisrun]][["houseman.A.present"]]){
						prop_mats<-c(prop_mats,"houseman2012")
					}
					if(getRuns()[[input$analysisrun]][["proportion.info.present"]]){
						prop_mats<-c(prop_mats,"experiment")
					}
				}
				selectInput('propMatrix', labl, prop_mats, selectize=TRUE)
				
			})
	
	
	output$lambdaSelector<-renderUI({
				LAMBDAS<-dataset()@parameters$lambdas
				LAMBDA.IDS<-1:length(dataset()@parameters$lambdas)
				names(LAMBDA.IDS)<-as.character(LAMBDAS)
				selectInput('lambda', 'Lambda value', LAMBDA.IDS)
			})
	

	
	output$minKselector<-renderUI({
				selectInput('minK', 'Minimum k:', dataset()@parameters$Ks)
			})
	
	
	output$maxKselector<-renderUI({
				selectInput('maxK', 'Maximum k:', dataset()@parameters$Ks, selected=dataset()@parameters$Ks[length(dataset()@parameters$Ks)])
			})
	
	output$minLambdaSelector<-renderUI({
				lambda_list<-sort(dataset()@parameters$lambdas)
				if (!is.null(input$lambdaMax)){
					lambda_list<-lambda_list[lambda_list<=input$lambdaMax]
				}
				selectInput('lambdaMin', 'Minimum lambda:', lambda_list, selected=lambda_list[which.min(lambda_list)])
			})
	output$maxLambdaSelector<-renderUI({
				lambda_list<-sort(dataset()@parameters$lambdas)
				if (!is.null(input$lambdaMin)){
					lambda_list<-lambda_list[lambda_list>=input$lambdaMin]
				}
				selectInput('lambdaMax', 'Maximum lambda:', lambda_list, selected=lambda_list[which.max(lambda_list)])
			})
	
	output$componentSelector<-renderUI({
				if (!is.null(input$K)){
					comps<-c(1:input$K, NA)
					#cat(file = stderr(), comps, 'Hello\n')
					names(comps)<-c(as.character(1:input$K), "sum best matching")
					#cat(file = stderr(), names, 'Hello\n')
				}
				else {
					comps<-c(1, NA)
					#cat(file = stderr(), comps, 'Hello\n')
					names(comps)<-c("1", "sum best matching")
					#cat(file = stderr(), names, 'Hello\n')
				}
				selectInput('component', 'LMC:', comps, selectize=TRUE, multiple=TRUE, selected=isolate({if("component" %in% names(input)) as.character(input$component) else 1}))
				#input$panel=="Proportions"
			})
	

	
	
	output$pointColorSelector<-renderUI({
				
				cats<-names(getCGcategories())
				feats<-names(getCGquantFeatureSettings())
				list(
						selectInput('pointCategory', 'Color data points by:', c("none", cats, feats), selectize=TRUE)#,							
						#checkboxInput('CGsubsetToCategory', "limit to", value=FALSE)
				)
			})
	
	
	output$pointFilter<-renderUI({
				
				#siteannot<-names(getCGcategories())
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
				
				if("CGsubsetToCategory" %in% names(input) && input$CGsubsetToCategory && input$pointCategory!="none"){
					cats<-getCGcategories()
							
					#cat_lists<-getCGcatAssignments()
					#catv<-cat_lists[[cat]][ind]
					#point.cols<-c("grey", rainbow(length(cats[[cat]])))[catv+1L]
					features<-1:length(cats[[input$pointCategory]])
					names(features)<-cats[[input$pointCategory]]
					
					selectInput('pointSubset', 'Show only:', features, selectize=TRUE, multiple=TRUE, selected=1)
				}
				
			})
	
	output$geneSetSelector<-renderUI({
				gene.sets<-getGeneSets()				
				selectInput('geneSet', 'Gene set:', names(gene.sets), selectize=TRUE)
				
			})
	
	output$locusSelector<-renderUI({
				gene.sets<-getGeneSets()
				selectInput('locus', 'Gene:', gene.sets[[input$geneSet]], selectize=TRUE)
				
			})
	
	output$sampleColorSelector<-renderUI({
				pd<-getPhenoData()
				if(!is.null(pd)){
					siteannot<-colnames(pd)
				}else{
					siteannot<-character()
				}
				
				selectInput('mdsDataCat', 'Color samples by:', c("none", siteannot), selectize=TRUE)							
				
			})
	
	output$refProfileSelector<-renderUI({
				
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
	
	output$performancePanel<-renderUI({
				
				if(input$performanceMode=="lineplots"){
					list(plotOutput('lineplot', 
									height = "800px",
									width = "600px"),
							br(),
							downloadLink("lineplotPDF", "PDF"))
				}else if(input$performanceMode=="table"){
					tableOutput('performanceTable')
				}
				
			})
	
	output$componentsPanel<-renderUI({
				
				if(input$componentPlotType=="mds plot" || input$componentPlotType=="dendrogram"){
					h="500px"
					w="500px"
				}else if(input$componentPlotType=="heatmap"){
					h="500px"
					w=sprintf("%dpx", max(500, 50*as.integer(input$K)))
				}else if(input$componentPlotType=="scatterplot matching"){
					h0=300
					w0=300
					ncol=min(3,as.integer(input$K))
					nrow=(as.integer(input$K) %/% min(3,as.integer(input$K))) + as.integer(as.integer(input$K) %% min(3,as.integer(input$K))>0)
					h=sprintf("%dpx", h0*nrow)
					w=sprintf("%dpx", h0*ncol)
				}else if(input$componentPlotType=="locus plot"){
					h=500
					w=1000
				}else{
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
				
			})
	#}

	output$metaanalysisPanel<-renderUI({
					
				w=500
				h=500
				if(input$analysisType=="compare LMCs"){
					
					#list(plotOutput('metaPlot',
					list(plotOutput('comparisonPlot',
								height = if(input$comparativePlotType=="dendrogram") h else 1.2*h,
								width =  if(input$comparativePlotType=="dendrogram") 2*w else 2.5*w), br()#,
						#downloadLink("metaPlotPDF", "PDF")
					)
				
				}else if(input$analysisType=="differential methylation"){
					
					list(plotOutput('diffCGPlot'),
						if(input$diffOutputType == "Table"){
							DT::dataTableOutput('diffCGTable')
						}else if(input$diffOutputType == "GREAT enrichments"){
							#input$GREATsubmitQuery
							#isolate({
								if(input$GREATresults == "enriched ontologies (table)"){
									isolate({
										DT::dataTableOutput('GREATenrichmentTable')
									})
								}else if(input$GREATresults == "enriched ontologies (plot)"){
									isolate({
										plotOutput('GREATenrichmentPlot',
											height=h, width = 2*w)
									})
								}else if(input$GREATresults == "region-gene associations (table)"){
									isolate({
												DT::dataTableOutput('GREATgeneRegionTable')
											})
								}else if(input$GREATresults == "region-gene associations (plot)"){
									isolate({
												plotOutput('GREATgeneRegionPlot',
														height=h, width = 2*w)
											})
								}else if(input$GREATresults == "region-gene associations for term"){
									isolate({
										DT::dataTableOutput('GREATgenes4TermTable')
									})
								}
							})
						#})
				}else if(input$analysisType=="phenotype modeling"){
					if(input$phenoModelOutput=="summary plot"){
						list(plotOutput('phenotypeModelPlot',
										height = h,
										width =  2*w), br()#,
						)
					}else if(input$phenoModelOutput=="single case"){
						verbatimTextOutput("phenotypeModelSummary")
					}
				
				}
				
			})
	
	output$compareRunSelector<-renderUI({
				#wellPanel(
				runs<-names(getRuns())
				if (!is.null(runs)){
					names(runs)<-paste(seq_len(length(runs)),runs, sep=". ")
				}
				selectInput('analysisrun_ref', '', runs, selectize=TRUE, width="750px", selected=1)
				#)		
			})

	output$topSDcgsSelector<-renderUI({
			
			gr<-as.integer(input$cg_group)
#			ind<-readRDS(sprintf("%s/cg_group_%d.RDS", 
#							getRuns()[[input$analysisrun]][["run.dir"]], 
#							#dataset()$groups[gr]
#							gr
#					))
			ind<-getCGsubset()
				
			sliderInput('topSDcpgs', 'Select top SD cgs', min=1, max=length(ind),
						value=length(ind), step=500, round=0)
			})
	
	output$topSDcgsSelectorCompare<-renderUI({
				
				gr<-as.integer(input$cg_group)
#				ind<-readRDS(sprintf("%s/cg_group_%d.RDS", 
#								getRuns()[[input$analysisrun]][["run.dir"]], 
#								#dataset()$groups[gr]
#								gr
#						))
				ind<-getCGsubset()
#				ind_ref<-readRDS(sprintf("%s/cg_group_%d.RDS", 
#								getRuns()[[input$analysisrun_ref]][["run.dir"]], 
#								#dataset()$groups[gr]
#								gr
#						))
				ind_ref<-getCGsubsetRef()
				ind_common<-intersect(ind, ind_ref)
				
				sliderInput('topSDcpgsCompare', 'Select top SD cgs', min=100, max=length(ind),
						value=length(ind), step=100, round=0)
			})
	
	output$analysisTokensInput<-renderUI({
				
				if(repomode){
					dn<-getRuns()[[input$analysisrun]][["DATASET"]]
					dn_ref<-getRuns()[[input$analysisrun_ref]][["DATASET"]]
				}else{
					dn<-"Target"
					dn_ref<-"Reference"
				}
				list(
						textInput("analysisToken", "Analysis token:", dn),
						textInput("refAnalysisToken", "Reference analysis token:", dn_ref)
				)
				
			})
	
	output$dmCGComponentSelector<-renderUI({
				if (!is.null(input$K)){
					cmp_choices=as.list(as.character(1:as.integer(input$K)))
					#choices[[as.integer(input$K)+1]]=as.character(1:as.integer(input$K))
					names(cmp_choices)=c(as.character(1:as.integer(input$K)))#, "All")
				}else{
					cmp_choices=as.list(c("1"))
					#choices[[as.integer(input$K)+1]]=as.character(1:as.integer(input$K))
					names(cmp_choices)=c("1")#, "All")
				}
						
				
				list(
					selectizeInput("componentGroup1", "Select LMCs:", 
						choices=cmp_choices, selected=1, multiple = TRUE),
					selectizeInput("componentGroup2", "Select LMCs to compare:", 
						choices=cmp_choices, selected=2,multiple = TRUE)
					)
				
			})
	
	output$GREAToptionsSelector<-renderUI({
				
				ui<-list(selectInput('GREATrule', "Gene association criterion:", FactorViz:::GREAT_CRITERIA, selected=
										isolate({if(!is.null(input$GREATrule)) input$GREATrule else 1})))
				ui<-c(ui, list(numericInput('GREATmaxDistance', "Maximal distance (kb):", min=0, max=1000, value=
												isolate({if(!is.null(input$GREATmaxDistance)) input$GREATmaxDistance else 10}))))
				if(input$GREATresults %in% c("enriched ontologies (table)", "enriched ontologies (plot)")) 
					ui<-c(ui,list(selectInput('GREATontology', "GREAT ontology:", FactorViz:::GREAT_ONTOLOGIES, selected=
													isolate({if(!is.null(input$GREATontology)) input$GREATontology else 1}))))
				if(input$GREATresults %in% c("enriched ontologies (table)")){
					ui<-c(ui,list(numericInput('GREATmaxPval', "Max p-value:", value=
													isolate({if(!is.null(input$GREATmaxPval)) input$GREATmaxPval else 0.01}))))
					if("GREATenrichmentTable_rows_selected" %in% names(input) && length(input$GREATenrichmentTable_rows_selected)>0){
						ui<-c(ui, list(actionButton("GREATgenes4Term", "Show associations")))
					}
				}
				if(input$GREATresults %in% c("region-gene associations (table)", "region-gene associations for term")){
					if(("GREATgeneRegionTable_rows_selected" %in% names(input) && length(input$GREATgeneRegionTable_rows_selected)>0) || 
							("GREATgenes4TermTable_rows_selected" %in% names(input) && length(input$GREATgenes4TermTable_rows_selected)>0)){
						ui<-c(ui, list(actionButton("GREATdiff4Locus", "Show methylation difference")))
					}
				}
				if(input$GREATresults=="enriched ontologies (plot)"){
					ui<-c(ui, list(sliderInput('topGREATontologies', 'Top results', min=5, max=100, step=5, value=
													isolate({if(!is.null(input$topGREATontologies)) input$topGREATontologies else 10}))))
				}
				#if(input$GREATresults %in% c("region-gene associations (table)", "region-gene associations (plot)"))
				#	ui<-NULL
				ui
			})
	
	output$targetVariableSelector<-renderUI({
				
				pheno<-getPhenoData()
				selectInput("phenoTarget", "Select target trait:", 
						choices=colnames(pheno), selected=1, multiple = FALSE)
				
			})
	
	output$adjustmentVariableSelector<-renderUI({
				
				pheno<-getPhenoData()
				if("phenoTarget" %in% names(input)){
					list(selectizeInput("phenoAdjust", "Select trait(s) to adjust for:", 
							choices=setdiff(colnames(pheno),input$phenoTarget), selected=1, multiple = TRUE),
					if(!is.numeric(pheno[[input$phenoTarget]])) 
								selectInput("zeroLevel", "Zero level:", 
										choices=unique(pheno[[input$phenoTarget]]), selected=1, multiple = FALSE) 
					else NULL)
				}else{
					list(NULL)
				}
				
			})
	if(repomode){
		output$groupSelectorRef<-renderUI({
			#results<-readRDS(file.path(getRuns()[[input$analysisrun]][["run.dir"]], "collected.results.RDS"))
			results<-dataset_ref()
			#results <-MeDeComSet(outputs=results$outputs, parameters=results$parameters, dataset_info=list(m=100, n=100))
			#cat(sprintf("VAR EXISTS %s", "cg_subsets" %in% names(getRuns()[[input$analysisrun]])),file='/tmp/output.shiny.test.out')
			gr_lists<-results@parameters$cg_subsets
			GROUPS<-1:length(gr_lists)
			if(is.null(names(gr_lists))){
				names(GROUPS)<-sapply(gr_lists, paste, collapse="_")
			}else{
				names(GROUPS)<-names(gr_lists)
			}
#			for(gr in GROUPS){
				#cat(sprintf("GROUP PATHS %s", .libPaths()),file='/tmp/output.shiny.test.out')
				#cat(sprintf("GROUP %d, NAME %s\n", gr, names(GROUPS[gr])) ,file='/tmp/output.shiny.test.out', append=TRUE)
#			}	
			selectInput('cg_group_ref', 'Technical CpG subset:', GROUPS, selectize=TRUE)
	})
	output$Kselector_ref<-renderUI({
		Ks<-dataset_ref()@parameters$Ks
		selectInput('K_ref', 'Number of LMCs (reference)', Ks, selectize=TRUE)
		})

	output$lambdaSelector_ref<-renderUI({
		LAMBDAS<-dataset_ref()@parameters$lambdas
		LAMBDA.IDS<-1:length(dataset_ref()@parameters$lambdas)
		names(LAMBDA.IDS)<-as.character(LAMBDAS)
		if (LAMBDA.IDS== NULL){
		} 
		selectInput('lambda_ref', 'Lambda value (reference)', LAMBDA.IDS)
		})
	
	output$componentSelectorRef<-renderUI({
			comps<-c(1:input$K_ref)
			#names(comps)<-c(as.character(1:input$K), "sum best matching")
			selectInput('component_ref', 'LMC (ref analysis):', comps, selectize=TRUE, multiple=TRUE, selected=isolate({
									if("component_ref" %in% names(input))as.character(input$component_ref) else 1
									}))
			#input$panel=="Proportions"
	})
}

	
	parse_session_dir<-function(directory){
		
		sess_files<-list.files(directory)
		
		dates<-file.info(file.path(directory, sess_files))[["mtime"]]
		
		sess_files<-sess_files[order(dates, decreasing = TRUE)]
		
		res<-strsplit(gsub(".RDS", "", sess_files), split="__")
		
		return(list(sess_files, res))
	}
	
	output$sessionsPanel<-renderUI({

				input$removeSession
				
				input$saveSession
				
				ui<-list()
				
				ui<-c(ui, list(titlePanel("Saved sessions:")))
				ui<-c(ui,list(br()))
				
				sessions<-parse_session_dir(SESSIONS_DIR)				
				
				sess_files<-sessions[[1]]
				names(sess_files)<-sapply(sessions[[2]], paste, collapse=", saved on ")
				
				ui<-c(ui, list(actionButton("loadSession", "Load selected session")))
				ui<-c(ui, list(actionButton("removeSession", "Remove selected session")))
				ui<-c(ui,list(br()))
				ui<-c(ui, list(radioButtons("sessionToLoad", NULL, 
						sess_files
						)))
				
				ui
			})
	
	
	
	### Plotting functions
	
	doKselectionPlot<-function(){


	cat("------------------------------------------------plotting function------------------------------------\n")	
		results<-dataset()
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

		sample_subset = NULL
		if(statistic=="rmse"){
			meth.data<-getMethData()
			if (repomode){
				if("SAMPLE_SUBSET"%in% names(getRuns()[[input$analysisrunanalysisrun]])){
					sample_subset<-getRuns()[[input$analysisrun]][["SAMPLE_SUBSET"]]
				}else{
					sample_subset<-1:ncol(meth.data)
				}
			}
		}
		MeDeCom:::plot.K.selection(results, statistic = statistic, Ks = as.numeric(Kvals),lambdas = as.numeric(lambdas), cg_subset = as.integer(cg_), sample_subset = sample_subset,cg_subsets = gr_list, KvsRMSElambdaLegend = TRUE, normalizedCVE = input$normalizedCVE, addPlotTitle = input$addPlotTitle)
	}
	
	if(!is.null(runPart) && runPart=="Kselection"){
		doKselectionPlot()
	}
	
	#output k selection plot 
	output$RMSEvsKplot<-renderPlot({
				
				doKselectionPlot()
				
			})
	#	
	output$RMSEvsKplotPDF<-downloadHandler(
			
			filename=function(){
				sprintf("RMSEvsKplot_ANALYSIS_%s_cgsubset_%s_lambdaval_%s.pdf", 
						input$analysisrun, input$cg_group, input$lambda)
			},
			
			content=function(con){
				pdf(con)					
				doKselectionPlot()
				dev.off()
			},
			contentType="application/pdf"
	)



	###LambdaPlot
	doLambdaPlot<-function(){
		results<-dataset()

		#cg subset
		gr_list <- results@parameters$cg_subsets
		gr<-as.integer(input$cg_group)
		k<-as.integer(input$K)
		cg_ <- gr_list[gr]

		#minLambda and maxLambda
		minLam = input$lambdaMin
		maxLam = input$lambdaMax
		

		scale = input$lambdaScale
		if (scale == "logarithmic"){
			scale = "log"
		}
		
		includeRMSE_T<-FALSE
		includeMAE_A<-FALSE
		includeDist2C_T<-FALSE
		if (!is.null(input$includeRMSE_T) && input$includeRMSE_T) {
			includeRMSE_T<-TRUE
		}
		if (!is.null(input$includeMAE_A)&& input$includeMAE_A) {
			includeMAE_A<-TRUE
		}
		if (!is.null(input$includeDist2C_T)&& input$includeDist2C_T) {
			includeDist2C_T<-TRUE
		}

	MeDeCom:::plot.lambda.selection(results, cg_subset = as.integer(cg_), K = k, minLambda = minLam, maxLambda =maxLam, scale= scale ,includeRMSE_T= includeRMSE_T, includeMAE_A = includeMAE_A, includeDist2C_T = includeDist2C_T )

	}
	
	if(!is.null(runPart) && runPart=="lambdaSelection"){
		doLambdaPlot()
	}

	#lineplot	
	output$lineplot <- renderPlot({
				doLambdaPlot()
			})
	
	output$lineplotPDF <- downloadHandler(
			
			filename=function(){
				sprintf("performancePlot_ANALYSIS_%s_cgsubset_%s_K_%s.pdf", 
						input$analysisrun, input$cg_group, input$K)
			},
			
			content=function(con){
				pdf(con, width=7, height=10)					
				doLambdaPlot()
				dev.off()
			},
			contentType="application/pdf"
	)					
	
	output$performanceTable<-renderTable({
				
				results<-dataset()
				#results <-MeDeComSet(outputs=results$outputs, parameters=results$parameters, dataset_info=list(m=100, n=100))
				
				gr<-as.integer(input$cg_group)
				
				elts<-PERFORMANCE_MEASURES
				
				output<-list()
				
				test_Ks<-input$K
		

				Ks<-results@parameters$Ks
				index <- match(test_Ks, Ks)
				
				for(elt in 1:length(elts)){
					min_lls<-integer(length(test_Ks))
					min_vals<-double(length(test_Ks))
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



	###ComponentPlot	
	
	doComponentPlot<-function(){
		results<-dataset()
		cmp<-as.integer(input$component)
		k <- as.integer(input$K)

		#find out the index of k in Ks
		Ks<-results@parameters$Ks

		index<-match(k, Ks)

		#cg subset
		gr_list <- results@parameters$cg_subsets
		gr<-as.integer(input$cg_group)
		cg_ <- gr_list[gr]

		#lambdas
		ll <- as.integer(input$lambda)
		lambdas <- results@parameters$lambdas
		lambda <- lambdas[ll]

		#Ks 
		K <- as.integer(input$K)
		Ks <- results@parameters$Ks 
	
		ind<-getCGsubset()
		trueT<-getTrueT()
		Tref<-trueT[ind,]
		#types
		type <- input$componentPlotType
		if (type == "mds plot"){
			type <- "MDS"
		}
		#that
		That<-results@outputs[[as.character(gr)]]$T[[index,ll]]


		#sample.characteristic	
		if(input$componentPlotType=="MDS"){
			if(input$mdsDataCat!="none"){
				pheno.data<-getPhenoData()
				data.ch<-pheno.data[[input$mdsDataCat]]
			}
		}
		else{						
			data.ch<-NULL
		}
		#top.cgs
		top.cgs <- NA
		if(input$componentPlotType %in% c("heatmap","dendrogram","MDS","similarity graph")){
			if(!is.null(input$cgVarSubset) && input$cgVarSubset){
				top.cgs<-getTTandTrefSDranking()[1:input$topSDcpgs]
			}else if(!is.null(Tref)){
				top.cgs<-1:nrow(Tref)
			}else{
				top.cgs<-1:nrow(That)
			}
			
		}

		#scatterplot
		if(input$componentPlotType %in% c("scatterplot matching","scatterplot all","scatterplot avg matching")){
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
		meth.data<-getMethData()
		if(type == "locus plot"){
			
			sample_subset<-getSampleSubset()
			if(input$locusChr!="" && input$locusStart!="" && input$locusEnd!=""){
				
				locus_params<-list()
				locus_params[["ann"]]<-getCGAnnot()
				locus_params[["ann.genes"]]<-getGeneAnnot()
				locus_params[["locus.name"]]=input$locusName
				locus_params[["locus.chr"]]=input$locusChr
				locus_params[["locus.start"]]=as.integer(input$locusStart)
				locus_params[["locus.end"]]=as.integer(input$locusEnd)
				locus_params[["locus.forward"]]=input$locusStrand=="+"
				#,comp.cols=c("red", "blue", "lightblue")
				locus_params[["flank.start"]]=as.integer(input$upstreamMinus)
				locus_params[["flank.end"]]=as.integer(input$downstreamPlus)
				locus_params[["Tstar"]]=if(input$locusIncludeTref) Tref else NULL
				locus_params[["D"]]=if(input$locusIncludeD) meth.data[ind, sample_subset] else NULL
				locus_params[["plot.genes"]]=TRUE
				
			}else{
				locus_params<-NULL
			}
		}
		plotLMCs(results, 
				type = type, 
				K =  input$K, 
				lambda= lambda , 
				cg_subset = cg_, 
				lmc = as.integer(input$component), 
				Tref = Tref, 
				distance = input$mdsDist , 
				center = input$correlationCentered, 
				n.most.var = NA, 
				D = getMethData()[ind, getSampleSubset()] ,
				sample.characteristic = data.ch , 
				scatter.matching = scatter.matching, 
				scatter.smooth = TRUE, 
				scatter.cg.feature = NULL)
				#locus.parameters = locus_params
	}
	
	
	if(!is.null(runPart) && runPart=="components"){
		doComponentPlot()
	}
	
	output$componentPlot<-renderPlot({
				doComponentPlot()
			})	
	
	
	output$componentPlotPDF <- downloadHandler(
			
			filename=function(){
				sprintf("componentPlot_ANALYSIS_%s_cgsubset_%s_K_%s_cmp_%s_refprof_%s.pdf", 
						input$analysisrun, input$cg_group, input$K, paste0(input$component), paste0(input$profile))
			},
			
			content=function(con){
				#pdf(con, width=7, height=10)					
				pdf(con)
				doComponentPlot()
				dev.off()
			},
			contentType="application/pdf"
	
	)					

	###proportionPlots

	doProportionPlot<-function(){

		results<-dataset()#$outputs [[input$K]]

		#cg subset
		gr_list <-results@parameters$cg_subsets
		gr<-as.integer(input$cg_group)
		cg_ <- gr_list[gr]

		#lambdas
		ll <- as.integer(input$lambda)
		lambdas <- results@parameters$lambdas
		lambda <- lambdas[ll]

		Aref<-getTrueA()

		if(input$componentPlotType=="MDS" || input$propPlotType == "heatmap"){
			if(input$mdsDataCat!="none"){
				pheno.data<-getPhenoData()
				data.ch<-pheno.data[[input$mdsDataCat]]
			}else{
				data.ch<-NULL
			}
		}
		else{						
			data.ch<-NULL
		}

		plotProportions(results, 
				type = input$propPlotType, 
				K = as.integer(input$K), 
				lambda = lambda, 
				cg_subset = cg_, 
				lmc = as.integer(input$component), 
				Aref = Aref , 
				ref.profile = as.integer(input$profile), 
				assignment.method = "pearson", 
				sample.characteristic=data.ch, 
				heatmap.clusterCols = input$propClusterCols, 
				heatmap.clusterRows = input$propClusterRows)

		
	}
	
	if(!is.null(runPart) && runPart=="proportions"){
		doProportionPlot()
	}
	
	output$proportionplot<-renderPlot({
				doProportionPlot()
			})
	
	
	output$proportionPlotPDF <- downloadHandler(
			
			filename=function(){
				sprintf("proportionPlot_ANALYSIS_%s_cgsubset_%s_K_%s_cmp_%s_refprof_%s.pdf", 
						input$analysisrun, input$cg_group, input$K, paste0(input$component), paste0(input$profile))
			},
			
			content=function(con){
				#pdf(con, width=7, height=10)					
				pdf(con)
				doProportionPlot()
				dev.off()
			},
			contentType="application/pdf"
	
	)
	
	###ComparisonPlot

	doComparisonPlot<-function(){
		
		if(length(input$compareMatrices)>0){
			
			cg_subset<-getCGsubset()
			cg_subset_ref<-getCGsubsetRef()
			sample_subset<-getSampleSubset()
			sample_subset_ref<-getRefSampleSubset()
			
			cpg_intersect<-intersect(cg_subset, cg_subset_ref)
			
			if(length(cpg_intersect)<1){
				return(invisible(NULL))
			}			
			
			
			cg_map<-match(cpg_intersect,cg_subset)
			cg_map_ref<-match(cpg_intersect, cg_subset_ref)
			
			#cg_map<-cg_subset
			
			mdd<-matrix(NA, nrow=length(cpg_intersect), ncol=0)
			
			results <-dataset()
			gr<-as.integer(input$cg_group)
			ll<-as.integer(input$lambda)
			K<-as.integer(input$K)
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
				#results<-dataset()
				#return(invisible(NULL))
				That<-dataset()@outputs[[as.character(gr)]]$T[[index,ll]][cg_map,]
				
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
				D<-getMethData()[cg_subset,sample_subset][cg_map,]
				sn<-getSampleNames()[sample_subset]
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
				Tref<-getTrueT()[cg_subset,][cg_map,]
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
				#results<-dataset()
				print(input$K_ref)

			#hier noch aendern das man nicht auf input_ref zugreift
				That<-dataset_ref()@outputs[[as.character(gr)]]$T[[input$K_ref,ll_ref]][cg_map_ref,]
				
				colnames(That)<-paste0(paste0(input$refAnalysisToken, "_LMC"), 1:ncol(That))
				if(input$correlationCentered){
					mdd<-cbind(mdd,sweep(That,1,rowMeans(That),"-"))
				}else{
					mdd<-cbind(mdd,That)
				}
				
			}
			
			if("refD" %in% input$compareMatrices){
				D<-getRefMethData()[cg_subset_ref,sample_subset_ref][cg_map_ref,]
				sn<-getRefSampleNames()[sample_subset_ref]
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
				Tref<-getRefTrueT()[cg_subset_ref,][cg_map_ref,]
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
			
			#if(input$mdsDist=="euclidean"){
				
			#}else if(input$mdsDist=="correlation"){
			#	d <- as.dist(1-cor(mdd, method="pearson"))
			#}
			
			#mdd<-cbind(That[top.cgs,], Tref[top.cgs,])
			
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
	}
	
	if(!is.null(runPart) && runPart=="componentComparison"){
		return(doComparisonPlot())
	}
	
	output$comparisonPlot <- renderPlot({
				doComparisonPlot();
			})
	
	output$comparisonPlotPDF <- downloadHandler(
			
			filename=function(){
				sprintf("comparisonPlot_ANALYSIS_%s_cgsubset_%s_K_%s_cmp_%s.pdf", 
						input$analysisrun, input$cg_group, input$K, paste0(input$component))
			},
			
			content=function(con){
				#pdf(con, width=7, height=10)					
				pdf(con)
				doComparisonPlot()
				dev.off()
			},
			contentType="application/pdf"
	
	)
	
	fitPhenotypeModel<-function(Ahat, pheno_data, target_var, adj_vars){
		
		largest<-which.max(rowMeans(Ahat))
		smallest<-which.min(rowMeans(Ahat))
		
		if(is.integer(pheno_data[[target_var]])){
			exclude<-is.na(pheno_data[[target_var]])
		}else if(is.double(pheno_data[[target_var]])){
			exclude<-is.na(pheno_data[[target_var]])
		}else if(is.factor(pheno_data[[target_var]])){
			exclude<-is.na(pheno_data[[target_var]]) | as.character(pheno_data[[target_var]])  %in% c("exclude", "Exclude", "exl", "Excl")
		}else if(is.character(pheno_data[[target_var]])){
			exclude<-is.na(pheno_data[[target_var]]) | pheno_data[[target_var]] %in% c("exclude", "Exclude", "exl", "Excl")
		}else{
			exclude<-rep(FALSE, nrow(pheno_data))
		}
		#dump("dataSet", file='/tmp/dataSet1.RDump')
		
		dataSet<-as.list(pheno_data[which(!exclude),,drop=FALSE])
		Ahat<-Ahat[,which(!exclude)]
		#dump("dataSet", file='/tmp/dataSet2.RDump')
		
		#dataSet$Diagnosis_quant<-as.integer(factor(dataSet[[target_var]], levels=c("C","AD")))
		if(!is.numeric(dataSet[[target_var]])){
			if(is.character(dataSet[[target_var]])){
				if(!any(is.na(as.integer(dataSet[[target_var]])))){
					dataSet[[target_var]]<-as.integer(dataSet[[target_var]])
				}else{
					dataSet[[target_var]]<-as.factor(dataSet[[target_var]])
				}
			}
			if(is.factor(dataSet[[target_var]])){
				target_levels<-levels(dataSet[[target_var]])
				target_levels<-c(input$zeroLevel, setdiff(target_levels, input$zeroLevel))
				dataSet[[target_var]]<-factor(dataSet[[target_var]], levels=target_levels)
				dataSet$target_quant<-as.integer(dataSet[[target_var]])
				dataSet$target_quant[dataSet$target_quant>1]<-2L
			}
		}else{
			dataSet$target_quant<-as.integer(dataSet[[target_var]])
		}
		
		if(input$discardLMC=="largest"){
			exc_lmc<-largest
		}else if(input$discardLMC=="smallest"){
			exc_lmc<-smallest
		}
		
		for(cmp in seq_len(nrow(Ahat))[-exc_lmc]){
			dataSet[[sprintf("LMC%d",cmp)]]<-Ahat[cmp,]
		}
		
		theModel<-as.formula(sprintf("target_quant~%s", 
						paste(c(sprintf("LMC%d", seq_len(nrow(Ahat))[-exc_lmc]), adj_vars),collapse="+")))		
		
		#		fit <- try(lme(theModel, random=~1|PLATE, 
		#						data=validationData_Pheno[ii,]), silent=TRUE)
		#		
		dump("dataSet", file='/tmp/dataSet.RDump')
		
		fit<-lm(theModel,  data=dataSet)
	}
	
	output$phenotypeModelSummary<-renderPrint({
		
		results<-dataset()
		K <-input$K
	#finds out the index of k in Ks
		Ks<-results$Ks
		index <- NULL
	for (i in 1:length(Ks)){
		if(Ks[i]==K){
			index <- as.numeric(i)
		}
	}	
		Ahat<-dataset()@outputs[[input$cg_group]]$A[[as.integer(index),as.integer(input$lambda)]]
		
		pheno_data<-getPhenoData()[getSampleSubset(),,drop=FALSE]
		
		target_var<-input$phenoTarget
		adj_vars<-input$phenoAdjust
		
		fit<-fitPhenotypeModel(Ahat, pheno_data, target_var, adj_vars)
		
		print(summary(fit))
		
	})
	
	doPhenotypeModelPlot<-function(){
		
		results<-dataset()
		lls<-sort(results@parameters$lambdas)
		Kvals<-results@parameters$Ks
	#finds out the index of k in Ks
		Ks<-results@parameters$Ks
		index <- NULL
	for (i in 1:length(Ks)){
		if(Ks[i]==Kvals){
			index <- as.numeric(i)
		}
	}	
		
		pheno_data<-getPhenoData()[getSampleSubset(),,drop=FALSE]
		
		pval_matrix<-matrix(0, ncol=length(Kvals),nrow=length(lls))
		#match_freq_matrix<-matrix(0, ncol=length(Kvals),nrow=length(lls))
		
		blue.cols<-colorRampPalette(c("white", "blue"))
		
		target_var<-input$phenoTarget
		adj_vars<-input$phenoAdjust
		
		for(k in Kvals){
			for(lambda in sort(lls)){
				
				Ahat<-results@outputs[[input$cg_group]]$A[[as.integer(as.character(index)),match(lambda, lls)]]
				
				fit<-fitPhenotypeModel(Ahat, pheno_data, target_var, adj_vars)
				
				sf<-summary(fit)
				global.p<-pf(sf$fstatistic[1], sf$fstatistic[2], sf$fstatistic[3], lower.tail=FALSE)
				min.cmp.p<-min(sf$coefficients[-1,4])
				
				if(input$modelPval=="overall"){
					pval_matrix[which(lls==lambda),which(Kvals==k)]<-global.p
				}else if(input$modelPval=="minimal"){
					pval_matrix[which(lls==lambda),which(Kvals==k)]<-min.cmp.p
				}
				#mincmp_pval_matrix[which(LAMBDA_GRID==lambda),which(Ks==k)]<-min.cmp.p
			}}
		N_COL_BINS<-10
		
		heatmap.2(#x=1:length(Kvals),y=1:length(lls),
				-log10(pval_matrix),
				cellnote=matrix(as.character(round(-log10(pval_matrix),3)),ncol=ncol(pval_matrix)),
				#col=gray.colors(N_COL_BINS-1, start=1, end=0, gamma=1.5),	
				col=blue.cols(N_COL_BINS-1),
				#breaks=(1:N_COL_BINS)*(1/N_COL_BINS), 
				#axes=FALSE,xlab="k", ylab="lambda",
				labCol=as.character(Kvals), labRow=sprintf("%g", lls), 
				#tick=FALSE, las=2,
				scale="none", trace="none", Colv=NA, Rowv=NA, 
				key.xlab="-log10(p)", key.title="")
#		for(ri in 1:nrow(pval_matrix)){
#			for(ci in 1:ncol(pval_matrix)){
#				text(x=ci,y=ri,pval_matrix[ci,ri])
#			}
#		}
		#axis(1, at = 1:ncol(pval_matrix), labels=as.character(Kvals), tick=FALSE)
		#axis(2, at = 1:nrow(pval_matrix), labels=sprintf("%g", lls),tick=FALSE, las=2)
		
	}
		
	output$phenotypeModelPlot<-renderPlot({
				doPhenotypeModelPlot();
			})


	output$phenotypeModelPlotPDF <- downloadHandler(
			
			filename=function(){
				sprintf("phenotypeModelPlot_ANALYSIS_%s_cgsubset_%s_K_%s_cmp_%s.pdf", 
						input$analysisrun, input$cg_group, input$K, paste0(input$component))
			},
			
			content=function(con){
				#pdf(con, width=7, height=10)					
				pdf(con)
				doPhenotypeModelPlot()
				dev.off()
			},
			contentType="application/pdf"
	
	)
	
	getDiffCGs<-reactive(quote({
		#values$change
		results<-dataset()
		gr<-as.integer(input$cg_group)
		ll<-as.integer(input$lambda)
		K<-input$K
		#finds out the index of k in Ks
		Ks<-results@parameters$Ks
			index <- NULL
		for (i in 1:length(Ks)){
			if(Ks[i]==K){
				index <- as.numeric(i)
			}
		}	
		
		cmp_group1<-as.integer(input$componentGroup1)

		That<-dataset()@outputs[[gr]]$T[[index,ll]]
		
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
		
				
		ind_hypo<-getCGsubset()[hypo_cgs]
		ind_hyper<-getCGsubset()[hyper_cgs]
				
		ann_hypo<-getCGAnnot()[ind_hypo,,drop=FALSE]
		ann_hyper<-getCGAnnot()[ind_hyper,,drop=FALSE]
						
		ann_hypo<-cbind(data.frame(ID=rownames(ann_hypo), Diff=meth_diff[hypo_cgs]), ann_hypo)
		ann_hyper<-cbind(data.frame(ID=rownames(ann_hyper), Diff=meth_diff[hyper_cgs]), ann_hyper)
		
		list(ann_hypo=ann_hypo, ann_hyper=ann_hyper)
				
	}), quoted=TRUE)

	if(!is.null(runPart) && runPart=="diffCGs"){
		return(getDiffCGs())
	}
	
	doDiffCGPlot<-function(){
		#values$change
		results<-dataset()
		gr<-as.integer(input$cg_group)
		ll<-as.integer(input$lambda)
		K<-input$K
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
		
		That<-dataset()@outputs[[gr]]$T[[index,ll]]
		
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
		
	}
		
	if(!is.null(runPart) && runPart=="diffPlot"){
		doDiffCGPlot()
	}
		
	output$diffCGPlot<-renderPlot({
				doDiffCGPlot();
			})
	
		
	output$diffCGTable<-DT::renderDataTable({
			#renderTable({
				#values$change
				diff_cgs<-getDiffCGs()
				
				if(input$diffTableType=="hypomethylated"){
					output<-diff_cgs[[1]]
				}else if(input$diffTableType=="hypermethylated"){
					output<-diff_cgs[[2]]
				}					
				
				output
				
			},rownames=FALSE, selection = 'single',
			
#		,
		
#		options = list(rowCallback = I(
#						'function(row, data) {
#								// Bold cells for those >= 5 in the first column
#								if (parseFloat(data[0]) >= 5.0)
#								$("td:eq(0)", row).css("font-weight", "bold");
#								}'
						#""
#				))
#		HTML("<script>$('#linkToSummary').click(function() {
#						tabs = $('.tabbable .nav.nav-tabs li')
#						tabs.each(function() {
#						$(this).removeClass('active')
#						})
#						$(tabs[1]).addClass('active')
#						
#						tabsContents = $('.tabbable .tab-content .tab-pane')
#						tabsContents.each(function() {
#						$(this).removeClass('active')
#						})
#						$(tabsContents[1]).addClass('active')
#						
#						$('#summary').trigger('change')
#						
#						})</script>
#						")
	
	)#, include.rownames=FALSE)
	
	#outputOptions(output, "diffCGTable", priority = -999)
	
	observeEvent(input$locusPlotSelected,{
				
				if(!is.null(input$diffCGTable_row_last_clicked)){
					
					updateTabsetPanel(session, "panel", selected="LMCs")
					updateSelectInput(session, "componentPlotType", selected="locus plot")
					updateSelectInput(session, "locusType", selected="arbitrary")
				
					if(input$diffTableType=="hypomethylated"){
						diff<-getDiffCGs()[[1]]
					}else{
						diff<-getDiffCGs()[[2]]
					}
					updateTextInput(session, "locusName", value=diff[input$diffCGTable_row_last_clicked[1L],"ID"])
					updateNumericInput(session, "locusChr", value=diff[input$diffCGTable_row_last_clicked[1L],"Chromosome"])
					updateNumericInput(session, "locusStart", value=diff[input$diffCGTable_row_last_clicked[1L],"Start"])
					updateNumericInput(session, "locusEnd", value=diff[input$diffCGTable_row_last_clicked[1L],"End"])
					updateNumericInput(session, "locusStrand", value=diff[input$diffCGTable_row_last_clicked[1L],"Strand"])
				}else{
					print("nothing selected")
				}
				
			})
	
	
	getGREATresults<-reactive(quote({
								
			input$GREATsubmitQuery			
			if(input$diffTableType=="hypomethylated"){
				diff<-getDiffCGs()[[1]]
			}else{
				diff<-getDiffCGs()[[2]]
			}
			
			if(nrow(diff)>0){
			
				bed<-diff[,c(3:5)]
				rownames(bed)<-diff[,c(1)]
				bg<-getCGAnnot()[getCGsubset(),c(1:3)]
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
	
	getGREATtable<-reactive(quote({
				#expr<-quote()
				#}else{
				res<-NULL
				withProgress(message="Querying GREAT...", value=0, 
						expr={
							incProgress(1/2, detail = "Submitted job")
							#isolate({
								job <- getGREATresults()
							#})
							incProgress(1/2, detail = "Fetching table")
							if(!is.null(job)){
								#isolate({
									res<-getEnrichmentTables(job, input$GREATontology)
									res<-res[[input$GREATontology]]
									res[res$Hyper_Raw_PValue<input$GREATmaxPval,]
								#})
							}
						}
				)
						#, quoted=TRUE)
				#}
	}), quoted=TRUE)
			

	if(!is.null(runPart) && runPart=="GREATtable"){
				
		return(getGREATtable())
	}
	
	output$GREATenrichmentTable<- DT::renderDataTable({
				
				if (input$GREATsubmitQuery == 0)
					return()
				
				getGREATtable()
				
			}, rownames=FALSE, selection = 'single')

	doGREATenrichmentPlot<-function(){
		
		res<-getGREATtable()
				
		to_plot<-res[1:min(input$topGREATontologies,nrow(res)),]
		par(mar=c(4,40,2,2))
		barplot(rev(-log10(to_plot$Hyper_Raw_PValue)), horiz=TRUE,
				,names=rev(to_plot$Name), las=1, xlab="-log10(p-value)", col="blue")
		
	}
	output$GREATenrichmentPlot <- renderPlot({
				
				if (input$GREATsubmitQuery == 0)
					return()
				
				input$topGREATontologies
				input$GREATontology
				
				isolate({
					doGREATenrichmentPlot();
				})
			})
	
	if(!is.null(runPart) && runPart=="GREATplot"){
		doGREATenrichmentPlot();
	}
	
	getGREATgeneRegionStats<-function(){
		
		isolate({
			if(length(input$GREATenrichmentTable_rows_selected)>0){
				target_term<-input$GREATenrichmentTable_rows_selected[1]
				#target_term<-getGREATtable()[input$GREATenrichmentTable_rows_selected[1], "ID"]
				target_ontology<-input$GREATontology
			}else{
				target_term<-NULL
				target_ontology<-NULL
			}
			
		})
	
		withProgress(message="Querying GREAT...", value=0, 
		expr={
			incProgress(1/2, detail = "Submitted job")
			#isolate({
			job<-getGREATresults()
			incProgress(1/2, detail = "Fetching tables")
			res<-plotRegionGeneAssociationGraphs(job, ontology=target_ontology, termID=target_term)
			res<-as.data.frame(res)
			res<-res[,c(8,1:3,6:7)]
			colnames(res)<-c("ID", "Chromosome", "Start", "End", "Gene", "Dist2TSS")
			#rownames(res)<-res[,1]
			res
			#})
			}
		)
	}

	output$GREATgeneRegionTable<-DT::renderDataTable({
				
				if (input$GREATsubmitQuery == 0)
					return()
				
				res<-NULL
				#isolate({
					dummypdf<-tempfile()
					pdf(dummypdf)#dummy pdf
					res<-getGREATgeneRegionStats()
					dev.off()
					unlink(dummypdf)
					res
				#})
		
			},rownames=FALSE, selection='single')
	
	observeEvent(input$GREATdiff4Locus,{
				
				updateSelectInput(session, "diffOutputType", selected="Table")
				
				if(input$GREATresults == "region-gene associations (table)"){
					selected_cg<-input$GREATgeneRegionTable_rows_selected[1L]
				}else{
					selected_cg<-input$GREATgenes4TermTable_rows_selected[1L]
				}
				
#				session$onFlushed(function(){
#					session$sendInputMessage("diffCGTable_rows_selected", list(value=selected_cg))
#				})
		
				#proxy<-DT::dataTableProxy("diffCGTable")
				#selectRows(proxy, selected_cg)
				
			})
	
	output$GREATgeneRegionPlot<-renderPlot({
				
				if (input$GREATsubmitQuery == 0)
					return()
				
				isolate({
				
					par(mfrow = c(1, 3))
					par(mar=c(5,3,2,2), mgp=c(4,1,0))
					par(cex=1.25)
					getGREATgeneRegionStats()
					
				})
				NULL
			})
	
	if(!is.null(runPart) && runPart=="GREATgeneRegionPlot"){
		par(mfrow = c(1, 3))
		return(getGREATgeneRegionStats())
	}
	
	output$GREATgenes4TermTable<-DT::renderDataTable({
				
				if(input$GREATgenes4Term==0){
					return()
				}
				isolate({
					dummypdf<-tempfile()
					pdf(dummypdf)#dummy pdf
					res<-getGREATgeneRegionStats()
					dev.off()
					unlink(dummypdf)
					res
				})
				
			}, rownames=FALSE, selection = 'single')
	
	observeEvent(input$GREATgenes4Term,{
				
				updateSelectInput(session, "GREATresults", selected="region-gene associations for term")
				
			})
	
	### download data
	
	output$downloadPanel<-renderUI({
				wellPanel(	
						h4("Input Data"),
						{sprintf("Target data (intensities) for CpG subset # %s",input$cg_group)},
						downloadLink('inputDataIntMat', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("Target data (ratios) for CpG subset # %s",input$cg_group)},
						downloadLink('inputDataMat', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("CpG subset # %s (with respect to the RnBeads HM450 annotation)",input$cg_group)},
						downloadLink('inputCGsubset', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("Sample information")},
						downloadLink('inputPheno', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("Sample subset")},
						downloadLink('inputSampleSubset', label = "(.MAT)", class = NULL),
						br(),
						{sprintf("Reference profiles (T^star) for CpG subset # %s ",input$cg_group)},
						downloadLink('inputRefDataMat', label = "(.MAT)", class = NULL),
						br(),
						if(!is.null(getTrueA())) {sprintf("Known proportion matrix (A^star)")},
						if(!is.null(getTrueA())) downloadLink('inputTrueA',label="(.MAT)"),
						hr(),
						h4("Results"),
						{ sprintf("Results for CpG subset # %s , k %s, lambda %g",
					    input$cg_group, input$K, dataset()@parameters$lambdas[as.integer(input$lambda)])},
						downloadLink('outputResults', label = "(.MAT)", class = NULL)#,
#						hr(),
#						h4("Meta-data"),
#						{sprintf("Complete HM450 annotation from RnBeads")},
#						downloadLink('metaHM450mat', label = "(.MAT)", class = NULL)
				)
				
			})
	
	output$inputDataMat<-downloadHandler(
			filename=function(){
				gr_lists<-getRuns()[[input$analysisrun]][["cg_subsets"]]
				group_names<-sapply(gr_lists, paste, collapse="_")
				sprintf("input_data_betas_cggroup_%s.mat",group_names[as.integer(input$cg_group)])
			},
			content=function(con){
				ind<-getCGsubset()
				writeMat(con, D=getMethData()[ind,])
			}
	)
	
	output$inputDataIntMat<-downloadHandler(
			filename=function(){
				gr_lists<-getRuns()[[input$analysisrun]][["cg_subsets"]]
				group_names<-sapply(gr_lists, paste, collapse="_")
				sprintf("input_data_M_U_cggroup_%s.mat",group_names[as.integer(input$cg_group)])
			},
			content=function(con){
				ind<-getCGsubset()
				#writeMat(con, D=getMethData()[ind,])
				system(sprintf("cp %s %s", file.path(getRuns()[[input$analysisrun]][["data.dir"]], "MandU.mat") ,con))
			}
	)
	
	output$inputPheno<-downloadHandler(
			filename=function(){
				sprintf("input_data_pheno_%s.mat",input$analysisrun)
			},
			content=function(con){
				writeMat(con, pheno=getPhenoData())
			}
	)
	
	output$inputSampleSubset<-downloadHandler(
			filename=function(){
				sprintf("input_data_sampleSubset_%s.mat",input$analysisrun)
			},
			content=function(con){
				writeMat(con, sampleSubset=getSampleSubset())
			}
	)
	
	output$inputRefDataMat<-downloadHandler(
			filename=function(){
				gr_lists<-getRuns()[[input$analysisrun]][["cg_subsets"]]
				group_names<-sapply(gr_lists, paste, collapse="_")
				sprintf("Tstar_cggroup_%s.mat",group_names[as.integer(input$cg_group)])
			},
			content=function(con){
				ind<-getCGsubset()
				writeMat(con, Tstar=getTrueT()[ind,])
			}
	)
	
	output$inputCGsubset<-downloadHandler(
			filename=function(){
				gr_lists<-getRuns()[[input$analysisrun]][["cg_subsets"]]
				group_names<-sapply(gr_lists, paste, collapse="_")
				sprintf("cggroup_%s.mat",group_names[as.integer(input$cg_group)])
			},
			content=function(con){
				ind<-getCGsubset()
				writeMat(con, CGsubset=ind)
			}
	)
	
	output$inputTrueA<-downloadHandler(
			filename=function(){
				sprintf("Astar.mat")
			},
			content=function(con){
				writeMat(con, Astar=getTrueA())
			}
	)
	
	output$outputResults<-downloadHandler(
			filename=function(){
				gr_lists<-getRuns()[[input$analysisrun]][["cg_subsets"]]
				group_names<-sapply(gr_lists, paste, collapse="_")
				ll<-as.integer(input$lambda)						
				sprintf("Results_cggroup_%s_K_%s_lambda_%g.mat",group_names[as.integer(input$cg_group)],input$K,dataset()@parameters$lambdas[ll])
			},
			content=function(con){
				gr<-as.integer(input$cg_group)
				ll<-as.integer(input$lambda)
				K<-input$K
				#finds out the index of k in Ks
				Ks<-results@parameters$Ks
				index <- NULL
				for (i in 1:length(Ks)){
					if(Ks[i]==Kvals){
							index <- as.numeric(i)
					}
				}	
				writeMat(con, That=dataset()@outputs[[gr]][["T"]][[index,ll]], 
						Ahat=dataset()@outputs[[gr]][["A"]][[index,ll]], 
						Fval=dataset()@outputs[[gr]][["Fval"]][index,ll],
						CVE=dataset()@outputs[[gr]][["cve"]][index,ll])
			}
	)
	
	output$metaHM450mat<-downloadHandler(
			filename=function(){
				sprintf("RnBeads_HM450_CpG_annotation.mat")
			},
			content=function(con){
				writeMat(con, HM450annot=getCGAnnot())
			}
	)
	
	prepare_input_env<-function(){
		save_env<-new.env(parent=emptyenv())
		print(names(input))
		for(input_name in names(input)){
			if(!input_name %in% c("GREATsubmitQuery","removeSession", "locusPlotSelected", "saveSession", "loadSession", "sessionToLoad", ".shinyURL", ".getTinyURL") && !grepl("diffCGTable_", input_name) && !grepl("ss-net-opt-", input_name) && !grepl("^\\.", input_name)){
				assign(input_name, input[[input_name]], envir=save_env)
			}
		}
		save_env
	}
	
	observeEvent(input$saveSession,{
				
				saveRDS(prepare_input_env(), file=file.path(SESSIONS_DIR, sprintf("%s%s__%s.RDS", if(input$sessionName!="") paste0(input$sessionName, "_") else "", input$analysisrun, format(Sys.time(), "%a_%b_%d_%H-%M-%S_%Y"))))
				
			})
	
	output$sessionRdump <- downloadHandler(
			
			filename=function(){
				#print(sprintf("sessionDump_%s_%s.Rdump", 
				#		input$analysisrun, format(Sys.time(), "%a_%b_%d_%H-%M-%S_%Y")))
				sprintf("sessionDump_%s_%s.Rdump", 
						input$analysisrun, format(Sys.time(), "%a_%b_%d_%H-%M-%S_%Y"))
			},
			
			content=function(con){
				input_env<-prepare_input_env()
				dump_env<-new.env(parent=emptyenv())
				assign("input", as.list(input_env), envir=dump_env)
				dump_out<-capture.output(dump(ls(dump_env), file="", envir=dump_env))
				dump_out<-gsub("^\\s+", "", dump_out)
				dump_out<-gsub(",", "\n,", dump_out)
				cat(dump_out,file=con, sep="")
			},
			contentType="text/rdump"
	)			
	
	
		
	observeEvent(input$removeSession,{
				
				unlink(file.path(SESSIONS_DIR, input$sessionToLoad))
				
			})
	
	# use the scheme from shinyURL to load the inputs from a saved session
	#
	initFromEnv<-function(session, input_env, self){
		observe({
					
			print("observer working")
			print(ls(input_env))
					
			invalidateLater(1000, session)
			
			input_env_copy <- as.environment(as.list(input_env, all.names=TRUE))
			
			## suspend if nothing to do
			if ( length(ls(input_env)) == 0L) {
				self$destroy()
				return(NULL)
			}
			
			## iterate through available inputs as long as there are any uninitialized values in queryValues
			## the expression below depends on inputs which is neccassary to restore dynamic UIs
			inputValues <- reactiveValuesToList(session$input, all.names=FALSE)
			updateValues <- intersect(names(inputValues), ls(input_env))
			queryIds <- match(updateValues, ls(input_env))
			inputIds <- match(updateValues, names(inputValues))
			
#			cat(sprintf("ANOTHER ROUND\n"),file='/tmp/output.shiny.test.new.out', append=TRUE)
#			
#			cat(sprintf("inputs %s\n",names(inputValues)),file='/tmp/output.shiny.test.new.out', append=TRUE)
#			cat(sprintf("queries %s\n",ls(input_env)),file='/tmp/output.shiny.test.new.out', append=TRUE)
#			cat(sprintf("quids %d\n",queryIds),file='/tmp/output.shiny.test.new.out', append=TRUE)
#			
			if ( length(queryIds) > 0 ) {
				equal<-sapply(queryIds, function(id) identical(get(ls(input_env)[id],envir=input_env), inputValues[[ls(input_env)[id]]]))
#				print("removed")
#				print(ls(input_env)[queryIds[equal]])
				remove(list=ls(input_env)[queryIds[equal]], envir=input_env)
				
				#input_env <<- input_env
				#queryValues <<- queryValues[-queryIds]
					#cat(sprintf("removing %s\n",ls(input_env)[queryIds]),file='/tmp/output.shiny.test.new.out', append=TRUE)
					
#				for(id in queryIds){
#					if(get(ls(input_env)[id], envir=input_env)==inputValues[[ls(input_env)[id]]]){
#						
#						remove(list=ls(input_env)[id], envir=input_env)
#					}
#				}
			}
			
#			## suspend if nothing to do
#		if ( all(equal)) {
#			self$suspend()
#			return(NULL)
#		}


		# First assign the parameter to NA to simulate the input change on the client side 
		# in the opposite case the client never sends a request for the inputs which are set too early 
		session$onFlush(function() {
			
			#print(date())
			#print(ls(input_env_copy)[queryIds])
			
			for(input_name in ls(input_env_copy)[queryIds]){
#				print("==============sending messages before")
#				print(input_name)
#				print(get(input_name, envir=input_env_copy))
				session$sendInputMessage(input_name,list(value=NA))
			}
			
		})
		#print(inputValues[["dmr_threshold"]])
		session$onFlushed(function() {
					
					#print(date())
					#print(ls(input_env_copy)[queryIds])
					
					for(input_name in ls(input_env_copy)[queryIds]){
#							print("================sending messages after")
#							print(input_name)
#							print(get(input_name, envir=input_env_copy))
							session$sendInputMessage(input_name,list(value=get(input_name, envir=input_env_copy)))
					}
					
				})
		#print(inputValues[["dmr_threshold"]])
		#shiny:::flushReact()
		invisible(NULL)
		}, priority=-99)
	}
	
	
	# destroy the session loading observer, since, some inputs are not yet initialized  
	# and the former never exits
	stopRefresh<-function(session, initializer, self){
		observe({
					
					# give 45 sec for loading all the inputs
					invalidateLater(45000, session)
					
					print("destroyer working")
					print(counter)
					if(counter>=1){
						isolate({
							initializer$destroy()
							self$destroy()
						})	
					}
					counter<<-counter+1
					return(invisible(NULL))
					
				}, priority=-9999)
	}
	
	## a simple counter to ensure that destroyer does not fire too early 
	counter<-0
	
	observeEvent(input$loadSession,{
				
				sess_input<-readRDS(file.path(SESSIONS_DIR, input$sessionToLoad))
				
				## remove great inputs, they are currently quite tricky
				
				remove(list=ls(sess_input)[grep("GREAT", ls(sess_input))], envir = sess_input)
				
				updateTabsetPanel(session, "page", selected="Explorer")
			
				init<-initFromEnv(session, sess_input, init)
				
				counter<<-0
				
				destroyer<-stopRefresh(session, init, destroyer)
				
				lapply(session$input, print)
				
			})
	
	if(!is.null(runPart)){
		output
	}
	
}

