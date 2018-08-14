
startFactorViz<-function(object, input.data=NULL) { 
	app<-shiny::shinyApp(ui=getFactorVizUI, server=FactorViz_serverFunc,
			onStart=function(){
				if(inherits(object, "MeDeComSet")){
					medecom_object<<-object
					medecom_ref_object<<-MeDeComSet(parameters=list(), output=list())
					if(is.list(input.data)){
						input_object<<-input.data
					}else if(inherits(input.data,"RnBSet")){
						input_object<<-list(
								pheno.data=pheno(input.data), 
								meth.data=meth(input.data),
								sample.names=samples(input.data)
						)
					} else {
						input_object<<-list(
								pheno.data=NULL, 
								meth.data=NULL,
								sample.names=NULL
								)
					}
					repomode<<-FALSE
					true_T_matrix<<-NULL
					true_T_matrix_ref<<-NULL
					true_A_matrix<<-NULL
					if(inherits(input.data,"RnBSet")){
						cg_annot_object<<-annotation(input.data)
						gene_annot_object<<-rnb.annotation2data.frame(rnb.get.annotation(type="genes"))
						prepared_annotation<<-prepare_feature_annotations(cg_annot_object)

					}else{
						cg_annot_object<<-NULL
						gene_annot_object<<-NULL
						prepared_annotation<<-list("cat_list"=NULL, "cat_inf_vs"=NULL, "features"=NULL, settings=NULL)
					}
					
					gene_sets_object<<-NULL
				}else if(is.character(object)){
					repo_dir_list<<-object
					if(is.null(names(repo_dir_list))){
						names(repo_dir_list)<-sprintf("Repository %d", 1:lenth(repo_dir_list))
					}
					repomode<<-TRUE
				}else{
					stop("wrong value supplied for object")
				}
				
			},
			options = c(port=7669))
	
	shiny::runApp(app)
	
}
