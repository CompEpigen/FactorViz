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


GREAT_CRITERIA<<-c("oneClosest", "twoClosest", "basalPlusExt")

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
				"ITERMAX"="Maximal numer of iterations"
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

#############################################
############ DeComp FILES ###################
#############################################

ANN_C<<-"ann_C.RData"
ANN_S<<-"ann_S.RData"
MEDSET<<-"medecom_set.RData"
REF<<-"ref_meth.RData"
CURRENT_USER<<-Sys.info()[["user"]]

#############################################
################ FLAGS ######################
#############################################

MEDSET_FLAG<<-F
ANN_C_FLAG<<-F
TRUE_T_FLAG<<-F
PHENO_DATA_FLAG<<-F
SAMPLE_NAME_FLAG<<-F
METH_DATA_FLAG<<-F
#############################################
############## Base Objects #################
#############################################

medecom_object<<-NULL
input_object<<-list(
  pheno.data=NULL,
  meth.data=NULL,
  sample.names=NULL
)

true_T_matrix<<-NULL
true_A_matrix<<-NULL
true_T_matrix_ref<<-NULL
cg_annot_object<<-NULL
gene_annot_object<<-NULL
prepared_annotation<<-list("cat_list"=NULL,
                           "cat_inf_vs"=NULL,
                           "faetures"=NULL,
                           "settings"=NULL
                           )
gene_set_object<<-NULL
