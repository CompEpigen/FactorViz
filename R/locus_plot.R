locus_plot<-function(
		That,
		cgs,
		cg.map=1:nrow(That),
		locus.chr="chr11"
		,locus.start=31798048
		,locus.end=31847802
		,locus.name="PAX6"
		,locus.forward=FALSE
		,comp.cols=rainbow(ncol(That))
		,legend.pos="topleft"
		,flank.start=1000
		,flank.end=1000
		,Tstar=NULL
		,D=NULL
		,plot.genes=FALSE
){
	
	require(RnBeads)
	ann<-rnb.annotation2data.frame(rnb.get.annotation("probes450"))
	
	ann.cgs<-ann[cgs,]
	cgs.locus<-which(ann.cgs$Chromosome==locus.chr & ann.cgs$Start>locus.start-flank.start & ann.cgs$End<locus.end+flank.end)
	ann.cgs.locus<-ann.cgs[cgs.locus, ]
	
	if(plot.genes){
		ann.genes<-rnb.annotation2data.frame(rnb.get.annotation("genes"))
		
		genes.locus.start<-which(ann.genes$Chromosome==locus.chr & ann.genes$Start>locus.start-flank.start & ann.genes$Start<locus.end+flank.end)
		genes.locus.end<-which(ann.genes$Chromosome==locus.chr & ann.genes$End>locus.start-flank.start & ann.genes$End<locus.end+flank.end)
		genes.locus.cover<-which(ann.genes$Chromosome==locus.chr & ann.genes$Start<locus.start-flank.start & ann.genes$End>locus.end+flank.end)
		ann.genes.locus<-ann.genes[unique(c(genes.locus.start, genes.locus.end, genes.locus.cover)), ]
	}
	
	
	plot(NA,NA, 
			type="n",lty=3,pch=15, col="white", 
			ylim=c(-0.3-plot.genes*0.2,1), 
			xlim=c(locus.start-flank.start, locus.end+flank.end), 
			ylab="methylation level", xlab=locus.chr,
			yaxt="n")
	axis(2, at=c(0,0.25,0.5,0.8,1))
	abline(h=0)
	for(cmp in 1:ncol(That)){
		lines(ann.cgs.locus$Start, That[cgs.locus,cmp], type="o",lty=2, pch=15, cex=1, col=comp.cols[cmp])
	}
	if(!is.null(Tstar)){
		prof.cols<-rainbow(ncol(That)+ncol(Tstar))[(ncol(That)+1):(ncol(That)+ncol(Tstar))]
		for(prof in 1:ncol(Tstar)){
			lines(ann.cgs.locus$Start, Tstar[cgs.locus,prof], type="o", lty=4, pch=19, cex=1, col=prof.cols[prof])
		}
	}else{
		prof.cols<-character()
	}
	if(!is.null(D)){
		for(prof in 1:ncol(D)){
			lines(ann.cgs.locus$Start, D[cgs.locus,prof], type="o", lty=3, pch=21, cex=0.5, col="lightgrey")
		}
	}
	arrows(if(locus.forward) locus.start else locus.end, 
			-0.15, 
			if(locus.forward) locus.end else locus.start, 
			-0.15, length = 0.1)
	text(locus.start+(locus.end-locus.start)/2, -0.2, locus.name)
	
	if(plot.genes && nrow(ann.genes.locus)>0){
		for(row in 1:nrow(ann.genes.locus)){
			print(nrow(ann.genes.locus))
			gene.forward<-ann.genes.locus[row,"Strand"]=="+"
			line_y_pos<-(-1)*(0.25)-(((row-1) %% min(4, nrow(ann.genes.locus))))*(0.25/min(4,nrow(ann.genes.locus)))
			print(line_y_pos)
			arrows(if(gene.forward) ann.genes.locus[row,"Start"] else ann.genes.locus[row,"End"], 
					line_y_pos, 
					if(gene.forward) ann.genes.locus[row,"End"] else ann.genes.locus[row,"Start"], 
					line_y_pos, length = 0.1)
			lab_pos<-ann.genes.locus[row,"Start"]+(ann.genes.locus[row,"End"]-ann.genes.locus[row,"Start"])/2
			if(lab_pos<locus.start-flank.start || lab_pos>locus.end+flank.end) 
				if(ann.genes.locus[row,"Start"]>locus.start-flank.start )				
					lab_pos<-(locus.end+flank.end+ann.genes.locus[row,"Start"])/2
				else if(ann.genes.locus[row,"End"]<locus.end)
					lab_pos<-(locus.start-flank.start+ann.genes.locus[row,"End"])/2
				else lab_pos<-((locus.start-flank.start)+(locus.end+flank.end))/2
			if(!is.na(ann.genes.locus[row,"symbol"]))
				text(lab_pos, line_y_pos-0.05, ann.genes.locus[row,"symbol"])
			else
				text(lab_pos, line_y_pos-0.05, ann.genes.locus[row,"ID"])
		}
	}
	legend(legend.pos, pch=c(rep(15,ncol(That)),if(!is.null(Tstar)) rep(20,ncol(Tstar)) ), 
			col=c(comp.cols,prof.cols), lty=c(rep(2, ncol(That)), if(!is.null(Tstar)) rep(4, ncol(Tstar))),
			legend=if(!is.null(colnames(That))) c(colnames(That), colnames(Tstar)) else c(sprintf("LMC %d", 1:ncol(That)), colnames(Tstar)))
	return(invisible(list(locus.data=That[cgs.locus,], locus.ann=ann.cgs.locus)))
}