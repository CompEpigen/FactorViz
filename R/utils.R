

NECESSARY_FILES<-c(
		"collected.results.RDS",
		"analysis_settings.RDump")

zero2oneCols<-c("#AD0021", "#AC0323", "#AC0626", "#AB0928", "#AB0C2A", "#AA0F2C", 
				"#A9122F", "#A91531", "#A81833", "#A81A35", "#A71D38", "#A6203A", 
				"#A6233C", "#A5263E", "#A52941", "#A42C43", "#A42F45", "#A33248", 
				"#A2354A", "#A2384C", "#A13B4E", "#A13E51", "#A04153", "#9F4455", 
				"#9F4757", "#9E495A", "#9E4C5C", "#9D4F5E", "#9C5260", "#9C5563", 
				"#9B5865", "#9B5B67", "#9A5E69", "#99616C", "#99646E", "#986770", 
				"#986A73", "#976D75", "#977077", "#967379", "#95767C", "#95787E", 
				"#947B80", "#947E82", "#938185", "#928487", "#928789", "#918A8B", 
				"#918D8E", "#909090", "#909090", "#8E8E90", "#8C8C90", "#8B8A90", 
				"#898790", "#878590", "#858390", "#84818F", "#827F8F", "#807D8F", 
				"#7E7B8F", "#7C788F", "#7B768F", "#79748F", "#77728F", "#75708F", 
				"#746E8F", "#726C8F", "#70698F", "#6E678E", "#6C658E", "#6B638E", 
				"#69618E", "#675F8E", "#655D8E", "#645A8E", "#62588E", "#60568E", 
				"#5E548E", "#5D528E", "#5B508E", "#594E8D", "#574B8D", "#55498D", 
				"#54478D", "#52458D", "#50438D", "#4E418D", "#4D3F8D", "#4B3C8D", 
				"#493A8D", "#47388D", "#45368D", "#44348C", "#42328C", "#40308C", 
				"#3E2D8C", "#3D2B8C", "#3B298C", "#39278C")

#lambdaCols<-c(
#			"#66A0DB","#56CA38","#CB65CD","#A4BF38","#777BDD","#DAAA31","#9A81C0",
#			"#4F8B29","#DA4187","#68C365","#DF83B9","#43C393","#DB512F","#44B5C2",
#			"#CF525D","#3D7F50","#9C5482","#837E28","#4A6D91","#B96F33")		
#		
		
lambdaCols<-c(
		"#E88C44","#7494F9","#69FA51","#DE5CBF","#1ADE70","#C63F83","#ADEF5A","#DFA6F7",
		"#63A412","#5CAFF3","#BF681F","#2388C1","#CE353A","#03537F","#B03F38","#8B9BD9",
		"#5E1F0E","#195B35","#6F3D76","#742548")

scan_run_repo<-function(run.repo){
	
	runs<-list()
	
	#cat(sprintf("RUN REPO %s\n\n\n", run.repo), append=FALSE, file='/tmp/output.shiny.test.out')
	
	run.names<-list.dirs(run.repo, full.names=FALSE)
		
	for(rid in 1:length(run.names)){
		
		#cat(sprintf("RUN %s\n", run.names[rid]), append=TRUE, file='/tmp/output.shiny.test.out')
		
		run.dir<-file.path(run.repo, run.names[rid])
		
		validRun<-TRUE
		
		
		present.files<-list.files(run.dir)
		
		if(!all(NECESSARY_FILES %in% present.files)){
			validRun<-FALSE
		}
	
		if(validRun){
			
			current_run<-list()
			
			current_run$run.dir<-run.dir
			
			#current_run$settings<-list()
					
			settings_list<-parse(file.path(run.dir, "analysis_settings.RDump"))	
			
			#cat(sprintf("EXPR 1 %s\n", as.character(settings_list[[1]])), append=TRUE, file='/tmp/output.shiny.test.out')
			
			settings_env<-new.env()
			
			for(se in settings_list){
				eval(se, envir=settings_env)
			}
			
			se<-reannotate_cg_groups(settings_env)
			
			for(var in ls(settings_env)){
				#cat(sprintf("VAR %s\n", var), append=TRUE, file='/tmp/output.shiny.test.out')
				current_run[[var]]<-get(var,envir=settings_env)
			}			
			
			current_run$data.dir<-file.path(GLOBAL_DATA_DIR, sprintf("%s_%s_%s", current_run$DATASET, current_run$DATA_SUBSET, current_run$NORMALIZATION))
			
			present.data.files<-list.files(current_run$data.dir)
			
			current_run$reference.info.present<-"trueT.RData" %in% present.data.files
			current_run$proportion.info.present<-"trueA.RData" %in% present.data.files
			current_run$houseman.A.present<-"Ahouseman2012.RData" %in% present.data.files
			current_run$houseman.A.2016.present<-"Ahouseman2016.RData" %in% present.data.files
			
			
			runs[[run.names[rid]]]<-current_run
			
		}
	}
	
	return(runs)
}

load_analysis<-function(
		analysis,
		sGr,
		sK,
		sL
){
	
	#require(AHMETPRO)
	require(MeDeCom)
	
	if(any(grepl("deep", R.utils:::System$getHostname()))){
		RUN_DIR<-"/DEEP_fhgfs/projects/plutsik/projects/parameter_tuning/analysis/"
		GLOBAL_DATA_DIR<-"/DEEP_fhgfs/projects/plutsik/projects/parameter_tuning/data/"
	}else if(any(grepl("t7600", R.utils:::System$getHostname()))){
		RUN_DIR<-"/home/lutsik/Documents/science/projects/heterogeneity/analysis/parameter_tuning/runs/"
		GLOBAL_DATA_DIR<-"/home/lutsik/Documents/science/projects/heterogeneity/data/parameter_tuning/"
	}else{
		RUN_DIR<-"/home/lutsik/Documents/science/projects/heterogeneity/analysis/parameter_tuning/runs/"
		GLOBAL_DATA_DIR<-"/home/lutsik/Documents/science/projects/heterogeneity/data/parameter_tuning/"
	}
	
	anres<-list()
	anres$cg_group<-sGr
	anres$K<-sK
	anres$lambda<-sL
	#############################################################
	anres$ANALYSIS<-analysis
	
	source(file.path(RUN_DIR, analysis, "analysis_settings.RDump"), local=TRUE)
	
	res<-readRDS(file.path(RUN_DIR, analysis,"collected.results.RDS"))
	
	ind<-readRDS(file.path(RUN_DIR, analysis, sprintf("cg_group_%d.RDS", sGr)))
	
	load(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_"), "data.set.RData"))
	
	anres$cgs<-ind
	
	if(!"SAMPLE_SUBSET" %in% ls()){
		SAMPLE_SUBSET<-1:ncol(meth.data)
	}
	
	anres$D<-meth.data[ind,SAMPLE_SUBSET]
	
	if("trueT.RData" %in% list.files(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_")))){
		load(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_"), "trueT.RData"))
		anres$Tstar<-trueT[ind,]
	}
	
	if("trueA.RData" %in% list.files(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_")))){
		load(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_"), "trueA.RData"))
		
		anres$Astar<-trueA[,SAMPLE_SUBSET]
	}
	
	if("Ahouseman2012.RData" %in% list.files(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_")))){
		load(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_"), "Ahouseman2012.RData"))
		
		#anres$Ahouseman2012<-Ahouseman2012[,SAMPLE_SUBSET]
		anres$Ahouseman2012<-Ahouseman[,SAMPLE_SUBSET]
	}
#	if("Thouseman2016.RData" %in% list.files(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_")))){
#		load(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_"), "Thouseman2016.RData"))
#		
#		anres$Thouseman2012<-Thouseman2012[ind,]
#	}
	
	if("pheno.RData" %in% list.files(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_")))){
		load(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_"), "pheno.RData"))
		
		anres$pheno<-pheno.data[SAMPLE_SUBSET,]
	}
	
	if("sample_ids.RDS" %in% list.files(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_")))){
		sample.ids<-readRDS(file.path(GLOBAL_DATA_DIR, paste(DATASET, DATA_SUBSET, NORMALIZATION, sep="_"), "sample_ids.RDS"))
		
		anres$ids<-sample.ids[SAMPLE_SUBSET]
	}
	
	sLL<-which(res$lambdas==sL)
	
	anres$That<-res$outputs[[as.character(sK)]]$T[[sGr,sLL]]
	anres$Ahat<-res$outputs[[as.character(sK)]]$A[[sGr,sLL]]
	
	#anres$Aprime<-AHMETPRO:::factorize.regr(anres$D, anres$Tstar)$A
	anres$Aprime<-MeDeCom:::factorize.regr(anres$D, anres$Tstar)$A
	
#rm(list=setdiff(ls(), "anres"))

	anres$Fval<-res$outputs[[as.character(sK)]]$Fval[[sGr,sLL]]
	anres$rmse<-res$outputs[[as.character(sK)]]$rmse[[sGr,sLL]]
	anres$rmseT<-res$outputs[[as.character(sK)]]$rmseT[[sGr,sLL]]
	anres$maeA<-res$outputs[[as.character(sK)]]$maeA[[sGr,sLL]]
	anres$cve<-res$outputs[[as.character(sK)]]$cve[[sGr,sLL]]
	anres$dist2C<-res$outputs[[as.character(sK)]]$dist2C[[sGr,sLL]]
	
	return(anres)
}


get_session_input<-function(session_dir, session_file){
	
	env<-readRDS(file.path(session_dir, session_file))
	
	input<-as.list(env)
	
	return(input)
	
}

extend_dataset_meta_info<-function(meta_info, data_dir){
	
	data_sets<-names(meta_info)
	
	for(set in data_sets){
		extra_files<-list.files(data_dir, pattern=sprintf("%s_data_subsets", set))
		if(length(extra_files)>0){
			for(extra_file in extra_files){
				new_data_subsets<-readRDS(file.path(data_dir, extra_file))
				meta_info[[set]][["data_subsets"]]<-c(meta_info[[set]][["data_subsets"]], new_data_subsets)
			}
		}
		
	}
	
	return(meta_info)
}


image.scale <- function(z, zlim, col = heat.colors(12),
		breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
	if(!missing(breaks)){
		if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
	}
	if(missing(breaks) & !missing(zlim)){
		breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
	}
	if(missing(breaks) & missing(zlim)){
		zlim <- range(z, na.rm=TRUE)
		zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
		zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
		breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
	}
	poly <- vector(mode="list", length(col))
	for(i in seq(poly)){
		poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	}
	xaxt <- ifelse(horiz, "s", "n")
	yaxt <- ifelse(horiz, "n", "s")
	if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
	if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
	if(missing(xlim)) xlim=XLIM
	if(missing(ylim)) ylim=YLIM
	plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
	for(i in seq(poly)){
		if(horiz){
			polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
		}
		if(!horiz){
			polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
		}
	}
}

####### CpG annotations

reannotate_cg_groups<-function(se){
	if("GROUP_LISTS" %in% ls(se)){
		group_lists<-get("GROUP_LISTS", envir=se)
		if(is.null(names(group_lists))){
			gnames<-character()
			
			for(group in group_lists){
				
				if(length(group)==16 && group==c(1:16)){
					gnames<-c(gnames, "All probes")
				}else if(length(group)==6 && group==c(6,7,8,14,15,16)){
					gnames<-c(gnames, "Type I probes")
				}else if(length(group)==10 && group==c(1:5,9:13)){
					gnames<-c(gnames, "Type II probes")
				}else{
					gnames<-c(gnames, paste(group, collapse = "_"))
				}
			}
			names(group_lists)<-gnames
			assign("GROUP_LISTS", group_lists, envir = se)
		}
	}
	return(se)
}


prepare_feature_annotations<-function(annot){
	
	output<-list()
	
	cats<-HM450_PROBE_CATEGORIES[["categorical"]]
	
	cat_list<-list()
	cat_int_vs<-list()
	
	for(cat in cats){
		allcats<-unique(annot[[cat]])
		cat_list[[cat]]<-allcats[!is.na(allcats)]
		vector<-as.integer(annot[[cat]])
		if(min(vector[!is.na(vector)])==0){
			vector<-vector+1
		}
		vector[is.na(vector)]<-0
		cat_int_vs[[cat]]<-vector
	}
	
	for(ext_file in list.files(sprintf("%s/hm450_extra_annotations/",GLOBAL_DATA_DIR))){
		ext_ann<-readRDS(file.path(sprintf("%s/hm450_extra_annotations/",GLOBAL_DATA_DIR),ext_file))
		cat<-gsub("\\.RDS", "", ext_file)
		cat<-gsub("_", " ", cat)
		
		allcats<-unique(ext_ann)
		cat_list[[cat]]<-allcats[!is.na(allcats)]
		vector<-as.integer(factor(ext_ann, levels=allcats))
		if(min(vector[!is.na(vector)])==0){
			vector<-vector+1
		}
		vector[is.na(vector)]<-0
		cat_int_vs[[cat]]<-vector
	}
	
	output[["cat_list"]]<-cat_list
	output[["cat_inf_vs"]]<-cat_int_vs
	
	feats<-HM450_PROBE_CATEGORIES[["quantitative"]]
	
	settings<-list()
	features<-list()
	
	for(feat in feats){
		
		settings[[feat]]<-list(min=min(annot[[feat]]), max=max(annot[[feat]]), class=typeof(annot[[feat]]))
		features[[feat]]<-annot[[feat]]
		
	}
	
	for(ext_file in list.files(sprintf("%s/hm450_extra_quant_features/",GLOBAL_DATA_DIR))){
		
		
		feature<-readRDS(file.path(sprintf("%s/hm450_extra_quant_features/",GLOBAL_DATA_DIR),ext_file))
		feat<-gsub("\\.RDS", "", ext_file)
		feat<-gsub("_", " ", feat)
		
		settings[[feat]]<-list(min=min(feature), max=max(feature), class=typeof(feature))
		features[[feat]]<-feature
		
	}
	
	output[["settings"]]<-settings
	output[["features"]]<-features
	
	if("HumanMethylation450" %in% colnames(annot)){
	
		probesTohg19<-which(!is.na(annot$HumanMethylation450))
		
		hg19ToProbes<-rep(NA, nrow(annot))
		hg19ToProbes[!is.na(annot$HumanMethylation450)]<-1:sum(!is.na(annot$HumanMethylation450))
		
		output[["hg19ToProbes"]]<-hg19ToProbes
	}
	
	return(output)
}

