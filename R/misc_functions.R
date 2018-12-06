load_data<-function(decomp_output=NULL, medecom_set=NULL, ann_C=NULL, ann_S=NULL, ref_meth=NULL){
	start_state_initialiser()
	path_setter(decomp_output=decomp_output, medecom_set=medecom_set, ann_C=ann_C, ann_S=ann_S, ref_meth=ref_meth)
	if (!is.null(PATH$MEDECOM_SET) && file.exists(PATH$MEDECOM_SET)) {
		new.envi <- new.env()
		load(PATH$MEDECOM_SET, envir=new.envi)
		medecom_object <<- get(ls(envir = new.envi),envir = new.envi)
		MEDSET_FLAG <<- T
	}
	#incProgress(1 / 4)
	if (!is.null(PATH$ANN_C) && file.exists(PATH$ANN_C)) {
		new.envi <- new.env()
		load(PATH$ANN_C, envir=new.envi)
		cg_annot_object <<- get(ls(envir = new.envi),envir = new.envi)
		ANN_C_FLAG <<- T
	}
	#incProgress(1 / 4)
	if (!is.null(PATH$ANN_S) && file.exists(PATH$ANN_S)) {
		new.envi <- new.env()
		load(PATH$ANN_S, envir=new.envi)
		input_object$pheno.data <<- get(ls(envir = new.envi),envir = new.envi)
		PHENO_DATA_FLAG <<- T
		if ("sample_id" %in% input_object$pheno.data) {
			input_object$sample.names <- input_object$pheno.data$sample_id
		} else{
			input_object$sample.names <- 1:nrow(input_object$pheno.data)
		}
		SAMPLE_NAME_FLAG <<- T
	}
	#incProgress(1 / 4)
	if (!is.null(PATH$REF_METH) && file.exists(PATH$REF_METH)) {
		new.envi <- new.env()
		load(PATH$REF_METH, envir=new.envi)
		true_T_matrix <<- get(ls(envir = new.envi),envir = new.envi)
		TRUE_T_FLAG <<- T
	}
	#incProgress(1 / 4)
	new.envi <- new.env()

	gc()
}



onstartLoad<-function(decomp_output=NULL, medecom_set=NULL, ann_C=NULL, ann_S=NULL, ref_meth=NULL ){
	global_variable_initialiser()
	start_state_initialiser()
	path_setter(decomp_output=decomp_output, medecom_set=medecom_set, ann_C=ann_C, ann_S=ann_S, ref_meth=ref_meth)
}

path_setter<-function(decomp_output=NULL, medecom_set=NULL, ann_C=NULL, ann_S=NULL, ref_meth=NULL){
	PATH<<-NULL
	if(!is.null(decomp_output)){
		if (substr(decomp_output, nchar(decomp_output), nchar(decomp_output))!= .Platform$file.sep){
			decomp_output=paste0(decomp_output, .Platform$file.sep)
		}
		print("Directory Input")
		PATH$BASE_PATH<<-decomp_output
		PATH$MEDECOM_SET<<-paste0(decomp_output, "medecom_set.RData")
		PATH$ANN_C <<- paste0(decomp_output,"ann_C.RData")
		PATH$ANN_S <<- paste0(decomp_output,"ann_S.RData")
		PATH$REF_METH <<- paste0(decomp_output,"ref_meth.RData")
		MULTIFILE<<-FALSE
	}else{
		if(!is.null(medecom_set) || !is.null(ann_C) || !is.null(ann_S) || !is.null(ref_meth)){
			print("Multi File Input")
			MULTIFILE<<-TRUE
		}else{
			print("No Input")
			MULTIFILE<<-FALSE
		}
		PATH$MEDECOM_SET<<-medecom_set
		PATH$ANN_C <<- ann_C
		PATH$ANN_S <<- ann_S
		PATH$REF_METH <<- ref_meth
	}
}
sanity_check<-function(){
	if(!inherits(medecom_object, "MeDeComSet")){
		return(c(FALSE, "Please provide a MeDeComSet"))
	}else if(!is.null(cg_annot_object) && (medecom_object@dataset_info$m!=nrow(cg_annot_object))){
		return(c(FALSE, "Please provide related CpG Annotation File"))
	}else if((!is.null(input_object$pheno.data)) && (medecom_object@dataset_info$n!=nrow(input_object$pheno.data))){
		return(c(FALSE, "Please provide related Sample Annotation File"))
	}else if((!is.null(true_T_matrix)) && (medecom_object@dataset_info$m!=nrow(true_T_matrix))){
		return(c(FALSE, "Please provide related Reference Methylome"))
	}else{
		return(c(TRUE, "ALL OK"))
	}
}

start_state_initialiser<-function(){

	CURRENT_USER <<- Sys.info()[["user"]]
	MEDSET_FLAG <<- F
	ANN_C_FLAG <<- F
	TRUE_T_FLAG <<- F
	TRUE_A_FLAG<<-F
	PHENO_DATA_FLAG <<- F
	SAMPLE_NAME_FLAG <<- F
	METH_DATA_FLAG <<- F
	medecom_object <<- NULL
	input_object <<- list(
		pheno.data = NULL,
		meth.data = NULL,
		sample.names = NULL
	)

	true_T_matrix <<- NULL
	true_A_matrix <<- NULL
	true_T_matrix_ref <<- NULL
	cg_annot_object <<- NULL
	gene_annot_object <<- NULL
	prepared_annotation <<- list(
		"cat_list" = NULL,
		"cat_inf_vs" = NULL,
		"faetures" = NULL,
		"settings" = NULL
	)
	gene_set_object <<- NULL
	medecom_ref_object<<-NULL
	lola.db<<-NULL
}



global_variable_initialiser<-function(){

	RUN_LIST_COLUMNS<<-c(
		"DATASET"="Data set"
		,"DATA_SUBSET"="Data subset"
	#	"NO_OF_SAMPLES"="Number of samples",
	#	"SAMPLE_SUBSET"="Sample subset (within data subset)",
	#	"REFERENCE_PROFILES"="Reference cell type profiles (Tstar)",
	#	"REFERENCE_PROFILES_SOURCE"="Source of the reference profiles",
	#	"NORMALIZATION"="Normalization method",
	#	"ANALYSIS_TOKEN"="Additional info",
	#	"LAMBDA_GRID"="Tested values of lambda",
	#	"GROUP_LISTS"="CpG subsets, used for analysis",
	#	"MARKER_SELECTION"="Further CpG selection steps",
	#	"Ks"="Tested values of k",
	#	"K_FIXED"="Reference profiles, supplied as fixed to the QuadHC_fixedT variant",
	#	"A_LOWER"="Lower bounds for the cell type proportions",
	#	"A_UPPER"="Upper bounds for the cell type proportions",
		,"ANALYSIS_DATE"="Analysis start time"
	#	"NINIT"="Number of random initializations",
	#	"NFOLDS"="Number of cross-validation folds",
	#	"ITERMAX"="Maximal numer of iterations"

	)


	PERFORMANCE_MEASURES<<-c("Objective"="Fval", "RMSE"="rmse", "CV error"="cve", "RMSE, T"="rmseT", "MDC, T"="dist2C", "MAE, A"="maeA")
	#PERFORMANCE_MEASURES<-c("Objective"="Fval", "CV error"="cve")


	HM450_PROBE_CATEGORIES<<-list(

				"categorical"=c(
						"Chromosome",
						"Strand",
						"Design",
						"Color",
						"Context",
						"CGI Relation",
						"SNPs 3",
						"SNPs 5",
						"SNPs Full"
				),

				"quantitative"=c(
						"CpG",
						"GC"
				)
			)

	GREAT_ONTOLOGIES<<-
			c(
					"GO Molecular Function",
					"GO Biological Process",
					"GO Cellular Component",
					"Mouse Phenotype",
					"Human Phenotype",
					"Disease Ontology",
					"MSigDB Oncogenic Signatures",
					"MSigDB Immunologic Signatures",
					"MSigDB Cancer Neighborhood",
					"Placenta Disorders",
					"PANTHER Pathway",
					"BioCyc Pathway",
					"MSigDB Pathway",
					"MGI Expression: Detected",
					"MSigDB Perturbation",
					"MSigDB Predicted Promoter Motifs",
					"MSigDB miRNA Motifs",
					"InterPro",
					"TreeFam",
					"HGNC Gene Families"
			)



	#
	# Set of meta-information tags for the deconvolution experiments
	#
	ANALYSIS_META_INFO<<-list(

			"analysis_params"=c(
					#"ANALYSIS"="Analysis name",
					"DATASET"="Data set",
					"DATA_SUBSET"="Data subset",
					"NO_OF_SAMPLES"="Number of samples",
					"SAMPLE_SUBSET"="Sample subset (within data subset)",
					"REFERENCE_PROFILES"="Reference cell type profiles (Tstar)",
					"REFERENCE_PROFILES_SOURCE"="Source of the reference profiles",
					"NORMALIZATION"="Normalization method",
					"ANALYSIS_TOKEN"="Additional info",
					"LAMBDA_GRID"="Tested values of lambda",
					#"GROUP_LISTS"="CpG subsets, used for analysis",
					"MARKER_SELECTION"="Further CpG selection steps",
					"Ks"="Tested values of k",
					"K_FIXED"="Reference profiles, supplied as fixed to the QuadHC_fixedT variant",
					"A_LOWER"="Lower bounds for the cell type proportions",
					"A_UPPER"="Upper bounds for the cell type proportions",
					"ANALYSIS_DATE"="Analysis start time",
					"NINIT"="Number of random initializations",
					"NFOLDS"="Number of cross-validation folds",
					"ITERMAX"="Maximal numer of iterations",
					"ASSEMBLY"="Genome Assembly"
			),

			"data_set_params"=c(
					"DATASET",
					"DATA_SUBSET",
					"REFERENCE_PROFILES"
			),

			"param_extensions"=list(
					"NORMALIZATION"=c(
							"unnorm"="No",
							"illumina"="Scaling to internal control probes, Illumina default method",
							"dasen"="dasen method from Pidsley et al., BMC Genomics, 2013",
							"noob"="Background corrected with method NOOB from the package methylumi",
							"external"= "External normalization and adjustment procedures"
					),

					"QUALITY_FILTERING"=c(
							"standard"=paste("Total intensity (M+U) outliers (not withn 0.05, 0.95 quantiles) were removed,",
									"all probes overlapping with SNPs were removed"),
							"snp"="All probes overlapping with SNPs were removed",
							"somatic"="All probes on chromosomes X and Y were removed",
							"snpANDsomatic"="All on chromosomes X and Y, and all probes overlapping with SNPs were removed"
					),

					"MARKER_SELECTION"=c(
							"no"="No marker selection. All CpGs used",
							#"houseman"="Only those CpGs were used, which also present in a lits of top-50k markers as selected by method in Houseman et al, BMC Bioinformatics, 2012"
							"pheno"="CpGs, showing the least association with the phenotypic covariates, based on the limma-fit",
							"houseman"="Only cell type-specific marker CpGs were used, selected with Houseman et. al., 2012 method",
							"houseman2014"="CpGs with a highly significant Delta coefficients from the RefFreeEWAS models, Houseman et. al., 2014 method",
							"random1k"="randomly selected 1000 CpGs",
							"random5k"="randomly selected 5000 CpGs",
							"random10k"="randomly selected 10,000 CpGs",
							"random25k"="randomly selected 25,000 CpGs",
							"random50k"="randomly selected 50,000 CpGs",
							"pca500"="union of 500 CpGs best scoring in each of 10 first principal components",
							"pca1k"="union of 1000 CpGs best scoring in each of 10 first principal components",
							"pca5k"="union of 5000 CpGs best scoring in each of 10 first principal components",
							"var1k"="1000 CpGs with highest s.d. across samples",
							"var5k"="5000 CpGs with highest s.d. across samples",
							"var10k"="10,000 CpGs with highest s.d. across samples",
							"var50k"="50,000 CpGs with highest s.d. across samples",
							"var100k"="100,000 CpGs with highest s.d. across samples",
							"hybrid1k"="500 CpGs with highest s.d. across samples and 500 CpGs randomly selected from the rest",
							"hybrid5k"="2500 CpGs with highest s.d. across samples and 2500 CpGs randomly selected from the rest",
							"hybrid10k"="5,000 CpGs with highest s.d. across samples and 5,000 CpGs randomly selected from the rest",
							"hybrid50k"="25,000 CpGs with highest s.d. across samples and  25,000 CpGs randomly selected from the rest",
							"rowFstat1k"="1,000 CpGs with highest association to the cell-type categorical in the reference data",
							"rowFstat5k"="5,000 CpGs with highest association to the cell-type categorical in the reference data",
							"rowFstat10k"="10,000 CpGs with highest association to the cell-type categorical in the reference data",
							"rowFstat15k"="15,000 CpGs with highest association to the cell-type categorical in the reference data",
							"rowFstat20k"="20,000 CpGs with highest association to the cell-type categorical in the reference data",
							"rowFstat50k"="20,000 CpGs with highest association to the cell-type categorical in the reference data"

					),

					"LAMBDA_GRID_TYPE"=c(
							"rough"="Rough",
							"fine"="Fine")

			)
	)
}
